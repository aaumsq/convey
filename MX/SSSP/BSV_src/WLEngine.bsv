package WLEngine;

import Vector           :: *;
import FIFOF            :: *;
import GetPut           :: *;
import ClientServer     :: *;
import Connectable      :: *;
import StmtFSM          :: *;
import FShow            :: *;
import BRAMFIFO         :: *;

import LFSR::*;

// ----------------
// BC library imports

import BC_Utils           :: *;
import BC_HW_IFC          :: *;
import BC_Transactors     :: *;

import BufBRAMFIFOF::*;
import GaloisTypes::*;
`include "GaloisDefs.bsv"

interface WLEngine;
    interface Vector#(`WL_ENGINE_PORTS, Put#(WLEntry)) streamIn;
    interface Vector#(`WL_ENGINE_PORTS, Get#(WLEntry)) streamOut;

    interface Vector#(16, Get#(MemReq)) memReq;
    interface Vector#(16, Put#(MemResp)) memResp;

    method Action init(BC_AEId fpgaId, BC_Addr lockLoc, BC_Addr headPtrLoc, BC_Addr tailPtrLoc, BC_Addr maxSize, BC_Addr bufferLoc);
    method Bool isDone();
endinterface


`define WLENGINE_NUM_BUFS 2
`define WLENGINE_BUFOUT_SIZE 512
`define WLENGINE_BUFIN_SIZE 512
`define WLENTRY_SIZE 8
`define LG_WLENTRY_SIZE 3
`define WLENGINE_BACKOFF 128
`define WRITEFSM_TIMEOUT 2048

(* synthesize *)
module mkWLEngine(WLEngine);
    // memOffset[0]: lock bit
    // memOffset[1]: headPtr
    // memOffset[2]: tailPtr
    // memOffset[3]: workList size
    // memOffset[4]: start of data
    
    Reg#(BC_AEId) fpgaId <- mkReg(0);
    Reg#(BC_Addr) lockLoc <- mkRegU;
    Reg#(BC_Addr) headPtr <- mkRegU;
    Reg#(BC_Addr) headPtrLoc <- mkRegU;
    
    Reg#(BC_Addr) tailPtr <- mkRegU;
    Reg#(BC_Addr) tailPtrLoc <- mkRegU;
    
    Reg#(BC_Addr) maxSize <- mkRegU;
    Reg#(BC_Addr) bufferLoc <- mkRegU;
    
    Reg#(Bool) started <- mkReg(False);
    Reg#(Bool) done <- mkReg(False);
    
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) reqQ <- replicateM(mkFIFOF);
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) respQ <- replicateM(mkFIFOF);
    
    Vector#(16, FIFOF#(MemReq)) memReqQ <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemResp)) memRespQ <- replicateM(mkFIFOF);
    
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) bufIn0 <- replicateM(mkSizedBRAMFIFOF(`WLENGINE_BUFIN_SIZE));
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) bufIn1 <- replicateM(mkSizedBRAMFIFOF(`WLENGINE_BUFIN_SIZE));
    Vector#(`WL_ENGINE_PORTS, Reg#(Bit#(1))) curBufIn <- replicateM(mkRegU);

    Vector#(`WL_ENGINE_PORTS, Vector#(2, FIFOF#(WLEntry))) doubleBufOut <- replicateM(replicateM(mkSizedBufBRAMFIFOF(`WLENGINE_BUFOUT_SIZE)));
    Vector#(`WL_ENGINE_PORTS, Reg#(Bit#(1))) curBufOut <- replicateM(mkRegU);


    Wire#(Bool) bufInValid <- mkDWire(False);
    Wire#(Bit#(`WL_LG_ENGINE_PORTS)) bufInWriteIdxW <- mkDWire(?);
    Wire#(Bit#(1)) bufInWriteBufW <- mkDWire(?);
    
    (* no_implicit_conditions *)
    rule setWires;
        function Bool bufInEmptyF0(Integer x) = !bufIn0[x].notEmpty;
        function Bool bufInEmptyF1(Integer x) = !bufIn1[x].notEmpty;
        Vector#(`WL_ENGINE_PORTS, Bool) bufInEmpties0 = genWith(bufInEmptyF0);
        Vector#(`WL_ENGINE_PORTS, Bool) bufInEmpties1 = genWith(bufInEmptyF1);
        let elem0 = findElem(True, bufInEmpties0);
        let elem1 = findElem(True, bufInEmpties1);
        if(isValid(elem0)) begin
            Bit#(`WL_LG_ENGINE_PORTS) idx = pack(fromMaybe(?, elem0));
            //$display("BufInEmpties: %b, Empty idx: %0d", bufInEmpties0, idx);
            bufInWriteIdxW <= idx;
            bufInWriteBufW <= 0;
            bufInValid <= True;
        end
        else if(isValid(elem1)) begin
            Bit#(`WL_LG_ENGINE_PORTS) idx = pack(fromMaybe(?, elem1));
            //$display("BufInEmpties: %b, Empty idx: %0d", idx);
            bufInWriteIdxW <= idx;
            bufInWriteBufW <= 1;
            bufInValid <= True;
        end
    endrule
    
    function BC_Addr getWLSize(BC_Addr head, BC_Addr tail, BC_Addr max);
        // Get number of entries in FIFO
        BC_Addr size = 0;
        if(head < tail) begin
            size = tail - head;
        end
        else if(head > tail) begin
            size = (max - tail) + head;
        end
        return size;
    endfunction
                                                          
    // Lock FSM: gets global FSM lock and updates head/tail pointers
    
    Reg#(Bit#(32)) lock_lockData <- mkRegU;
    let lockFSM <- mkFSM(
       seq
           action
               lock_lockData <= 32'b1;
           endaction
           
           // Obtain global worklist lock
           while(lock_lockData == 32'd1) seq
               action
                   if(`DEBUG) $display("%0d: mkWLEngine[%0d]: Reading lock bit at addr: %0x...", cur_cycle, fpgaId, lockLoc);
                   memReqQ[0].enq(tagged MemCAS32{addr: lockLoc, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: ?}, cmpVal: 0, swapVal: 1});
               endaction
           
               action
                   MemResp rsp = memRespQ[0].first();
                   memRespQ[0].deq();
                   lock_lockData <= truncate(rsp.data);
                   if(`DEBUG) $display("%0d: mkWLEngine[%0d]: old lock bit = %0d", cur_cycle, fpgaId, rsp.data);
                   // Data is the old data, so if 1 then it failed
                   if(truncate(rsp.data) == 32'd1) begin
                       //$display("  Someone currently using it, retry...");
                   end
               endaction
          endseq
           
           // Get updated head and tail pointers
           action
               //$display("%0d: mkWLEngine[%0d]: getting updated head/tail ptrs", cur_cycle, fpgaId);
               GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: ?};
               memReqQ[0].enq(tagged MemRead64{addr: headPtrLoc, gaddr: gaddr});
               memReqQ[1].enq(tagged MemRead64{addr: tailPtrLoc, gaddr: gaddr});
           endaction
           
           action
               MemResp headRsp = memRespQ[0].first();
               MemResp tailRsp = memRespQ[1].first();
               memRespQ[0].deq();
               memRespQ[1].deq();
               if(`DEBUG) $display("%0d: mkWLEngine[%0d]: headPtr: %0x, tailPtr: %0x", cur_cycle, fpgaId, headRsp.data, tailRsp.data);
               headPtr <= truncate(pack(headRsp.data));
               tailPtr <= truncate(pack(tailRsp.data));
           endaction
       endseq
       );
    
    // Write FSM: Writes all full buffers
    FIFOF#(Bit#(5)) writeFSM_writeQ <- mkSizedFIFOF(16);
    Reg#(Bit#(`WL_LG_ENGINE_PORTS)) writeFSM_curIdx <- mkRegU;
    Reg#(Bit#(1)) writeFSM_curBufIdx <- mkRegU;
    Reg#(Bool) writeFSM_done <- mkRegU;
    Reg#(Bool) writeFSM_issuedStore <- mkRegU;
    Reg#(BC_Addr) writeFSM_wlSize <- mkRegU;
    Reg#(BC_Addr) writeFSM_maxSizeMinusOne <- mkRegU;
    let writeFSM <- mkFSM(
       seq
           action
               if(`DEBUG) $display("%0d: mkWLEngine: WriteFSM START", cur_cycle);
               lockFSM.start();
               writeFSM_curIdx <= 0;
               writeFSM_done <= False;
               writeFSM_issuedStore <= False;
           endaction
           
           action
               if(`DEBUG) $display("%0d: mkWLEngine: WriteFSM lock + head/tailPtr obtained! headPtr=%0d, tailPtr=%0d", cur_cycle, headPtr, tailPtr);
               lockFSM.waitTillDone();
           endaction
           
           action
               writeFSM_wlSize <= getWLSize(headPtr, tailPtr, maxSize);
               writeFSM_maxSizeMinusOne <= maxSize-1;
           endaction
           
           while(!writeFSM_done) seq
               action
                   if(`DEBUG) $display("%0d: mkWLEngine WriteFSM checking doubleBufOut[%0d][%0d], writeFSM_done: %0d, notEmpty: %0d", cur_cycle, writeFSM_curIdx, writeFSM_curBufIdx, writeFSM_done, doubleBufOut[writeFSM_curIdx][writeFSM_curBufIdx].notEmpty);
                   if(doubleBufOut[writeFSM_curIdx][writeFSM_curBufIdx].notEmpty) begin
                       if(writeFSM_wlSize < writeFSM_maxSizeMinusOne) begin
                           WLEntry entry = doubleBufOut[writeFSM_curIdx][writeFSM_curBufIdx].first();
                           doubleBufOut[writeFSM_curIdx][writeFSM_curBufIdx].deq();
                          
                           BC_Addr addr = bufferLoc + (tailPtr << `LG_WLENTRY_SIZE);
                           memReqQ[4].enq(tagged MemWrite64{addr: addr, gaddr: GaloisAddress{mod: MK_WORKLIST, addr: ?}, data: pack(entry)});
                          
                           if(`DEBUG) $display("mkWLEngine: WriteFSM headPtr=%0d, tailPtr=%0d, packet: %x", headPtr, tailPtr, entry);
                           tailPtr <= tailPtr + 1;
                           writeFSM_wlSize <= writeFSM_wlSize + 1;
                           writeFSM_issuedStore <= True;
                       end
                       else begin
                           if(`DEBUG) $display("mkWLEngine WorkList is FULL! Breaking...");
                           writeFSM_done <= True;
                       end
                   end
                   else begin
                       if(`DEBUG) $display("mkWLEngine WriteFSM done with idx %0d", writeFSM_curIdx);
                       if(writeFSM_curIdx != fromInteger(`WL_ENGINE_PORTS-1)) begin
                           writeFSM_curIdx <= writeFSM_curIdx + 1;
                           if(`DEBUG) $display("mkWLEngine WriteFSM incrementing writeFSM_curIdx: %0d", writeFSM_curIdx+1);
                       end
                       else begin
                           if(`DEBUG) $display("%0d: mkWLEngine: WriteFSM writeFSM done!", cur_cycle);
                           writeFSM_done <= True;
                       end
                   end
               endaction
                   
               action
                   // TODO: Allow multiple writes in flight
                   if(writeFSM_issuedStore) begin
                       if(`DEBUG) $display("%0d: mkWLEngine: WriteFSM write entry finished", cur_cycle);
                       memRespQ[4].deq();
                       writeFSM_issuedStore <= False;
                   end
               endaction
           endseq
           
           // Write head and tailPtrs
           action
               if(`DEBUG) $display("%0d: mkWLEngine[%0d]: readFSM writing new head/tail ptrs, headPtr=%0d, tailPtr=%0d", cur_cycle, fpgaId, headPtr, tailPtr);
               GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: ?};
               memReqQ[4].enq(tagged MemWrite64{addr: headPtrLoc, gaddr: gaddr, data: extend(headPtr)});
               memReqQ[5].enq(tagged MemWrite64{addr: tailPtrLoc, gaddr: gaddr, data: extend(tailPtr)});
           endaction
           
           // When head/tailPtr writes complete, unlock
           action
               memRespQ[4].deq();
               memRespQ[5].deq();
               memReqQ[4].enq(tagged MemWrite32{addr: lockLoc, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: ?}, data: 0});
               writeFSM_curBufIdx <= writeFSM_curBufIdx + 1;
           endaction
           
           action
               memRespQ[4].deq();
               if(`DEBUG) $display("%0d: mkWLEngine[%0d]: WriteFSM done, unlocking!", cur_cycle, fpgaId);
           endaction
       endseq
       );
    
    // Read FSM: Fills one buffer
    Reg#(BC_Addr) readFSM_numEntries <- mkRegU;
    Reg#(BC_Addr) readFSM_curEntry <- mkRegU;
    Reg#(Bit#(10)) readFSM_backOff <- mkRegU;
    Reg#(Bit#(`WL_LG_ENGINE_PORTS)) readFSM_bufIdx <- mkRegU;
    Reg#(Bit#(1)) readFSM_buf <- mkRegU;
    Reg#(Bool) readFSM_success <- mkRegU;
    let readFSM <- mkFSM(
       seq
           action
               readFSM_bufIdx <= bufInWriteIdxW;
               readFSM_buf <= bufInWriteBufW;
               readFSM_success <= False;
               lockFSM.start();
               if(`DEBUG) $display("%0d: mkWLEngine[%0d]: Starting readFSM, filling buf %0d idx %0d...", cur_cycle, fpgaId, bufInWriteBufW, bufInWriteIdxW);
           endaction
           
           action
               lockFSM.waitTillDone();
           endaction
       
           action
               // Get number of entries in FIFO
               BC_Addr size = 0;
               if(headPtr < tailPtr) begin
                   size = tailPtr - headPtr;
               end
               else if(headPtr > tailPtr) begin
                   size = (maxSize - tailPtr) + headPtr;
               end
               
               // Determine number of entries to read
               BC_Addr entries = ?;
               if(size > `WLENGINE_BUFIN_SIZE) begin
                   entries = `WLENGINE_BUFIN_SIZE;
               end
               else begin
                   entries = size;
               end
               if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM headPtr: %0d, tailPtr: %0d, WL size: %0d, reading %0d entries", cur_cycle, fpgaId, headPtr, tailPtr, size, entries);
               readFSM_numEntries <= entries;
               readFSM_curEntry <= 0;
           endaction
           
           // Read entries and send to buffer
           if(readFSM_numEntries > 0) seq
               while(readFSM_curEntry < readFSM_numEntries) seq
                   action
                       if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM Reading entry %0d of %0d...", cur_cycle, fpgaId, readFSM_curEntry, readFSM_numEntries);
                       BC_Addr addr = bufferLoc + (headPtr << `LG_WLENTRY_SIZE);
                       memReqQ[2].enq(tagged MemRead64{addr: addr, gaddr: GaloisAddress{mod: MK_WORKLIST, addr: ?}});
                       
                       readFSM_curEntry <= readFSM_curEntry + 1;
                       
                       if(headPtr == (maxSize-1)) begin
                         headPtr <= 0;
                       end
                       else begin
                         headPtr <= headPtr + 1;
                       end
                   endaction
                   
                   action
                       MemResp rsp = memRespQ[2].first();
                       memRespQ[2].deq();
                       
                       WLEntry entry = unpack(rsp.data);
                       if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM entry priority: %0x, graphId: %0x", cur_cycle, fpgaId, tpl_1(entry), tpl_2(entry));
                       if(readFSM_bufIdx == 0)
                         bufIn0[readFSM_bufIdx].enq(entry);
                       else
                         bufIn1[readFSM_bufIdx].enq(entry);
                   endaction
               endseq
               
               // Write head and tailPtrs
               action
                   if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM writing new head/tail ptrs, headPtr=%0d, tailPtr=%0d", cur_cycle, fpgaId, headPtr, tailPtr);
                   GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: ?};
                   memReqQ[2].enq(tagged MemWrite64{addr: headPtrLoc, gaddr: gaddr, data: extend(headPtr)});
                   memReqQ[3].enq(tagged MemWrite64{addr: tailPtrLoc, gaddr: gaddr, data: extend(tailPtr)});
               endaction
               
               // When head/tailPtr writes complete, unlock
               action
                   memRespQ[2].deq();
                   memRespQ[3].deq();
                   memReqQ[2].enq(tagged MemWrite32{addr: lockLoc, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: ?}, data: 0});
               endaction
               
               action
                   if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM done, unlocking!", cur_cycle, fpgaId);
                   memRespQ[2].deq();
                   readFSM_success <= True;
               endaction
           endseq
           else seq
               // No data, unlock and stall avoid needless contention
               action
                   memReqQ[2].enq(tagged MemWrite32{addr: lockLoc, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: ?}, data: 0});
                   if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM done, unlocking!", cur_cycle, fpgaId);
               endaction
               
               action
                   memRespQ[2].deq();
               endaction
               
               if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM nothing to read, stalling for %0d cycles", cur_cycle, fpgaId, `WLENGINE_BACKOFF);
               readFSM_backOff <= 0;
               while(readFSM_backOff < `WLENGINE_BACKOFF) seq
                   action
                       readFSM_backOff <= readFSM_backOff + 1;
                   endaction
               endseq
               
               action
                   noAction;
               endaction
           endseq
       endseq
       );
    
    rule readWorklist(started);
        if(bufInValid) begin
            readFSM.start();
        end
    endrule
    
    Reg#(Bit#(16)) triggerWriteFSM_timeout <- mkRegU;
    Reg#(Bit#(1)) triggerWriteFSM_lastIdx <- mkRegU;
    rule triggerWriteFSM(started && writeFSM.done);
        function Bool bufOutFullF(Integer x) = !doubleBufOut[x][writeFSM_curBufIdx].notFull;
        Vector#(`WL_ENGINE_PORTS, Bool) bufOutFulls = genWith(bufOutFullF);
        function Bool isTrueF(Bool x) = x;
        if(any(isTrueF, bufOutFulls)) begin
            //$display("WLEngine triggerWriteFSM EngineQs[%0d] full: %0b", writeFSM_curIdx, bufOutFulls);
            writeFSM.start();
            triggerWriteFSM_timeout <= 0;
        end
        else begin
            //$display("WLEngine triggerWriteFSM curBuf: %0d full: %b", writeFSM_curBufIdx, bufOutFulls);
            if(triggerWriteFSM_timeout > `WRITEFSM_TIMEOUT) begin
                triggerWriteFSM_timeout <= 0;
                triggerWriteFSM_lastIdx <= triggerWriteFSM_lastIdx + 1;
                writeFSM_curBufIdx <= triggerWriteFSM_lastIdx;
                writeFSM.start();
                //$display("Triggering WriteFSM...");
            end
            else begin
                writeFSM_curBufIdx <= writeFSM_curBufIdx + 1;
                triggerWriteFSM_timeout <= triggerWriteFSM_timeout + 1;
            end
        end
    endrule
    

    
    for(Integer i = 0; i < `WL_ENGINE_PORTS; i = i + 1) begin
        
        rule setCurBufIn(started);
            if(((curBufIn[i] == 0) && !bufIn0[i].notEmpty && bufIn1[i].notEmpty) ||
               ((curBufIn[i] == 1) && !bufIn1[i].notEmpty && bufIn0[i].notEmpty)) begin
               // Flip bit
               curBufIn[i] <= 1 + curBufIn[i];
            end
            
            if((curBufIn[i] == 0) && bufIn0[i].notEmpty) begin
                WLEntry pkt = bufIn0[i].first();
                if(`DEBUG) $display("%0d: mkWLEngine curBufIn[0] packet %x streaming to WorkListFIFOF", cur_cycle, pkt);
                bufIn0[i].deq();
                respQ[i].enq(pkt);
            end
            if((curBufIn[i] == 1) && bufIn1[i].notEmpty) begin
                WLEntry pkt = bufIn1[i].first();
                if(`DEBUG) $display("%0d: mkWLEngine curBufIn[1] packet %x streaming to WorkListFIFOF", cur_cycle, pkt);
                bufIn1[i].deq();
                respQ[i].enq(pkt);
            end
        endrule
        
        rule streamToBuf(started);
            WLEntry entry = reqQ[i].first();
            reqQ[i].deq();
            
            Bit#(1) curBuf = curBufOut[i];
            
            if(doubleBufOut[i][curBuf].notFull) begin
                //$display("mkWLEngine reqQ->doubleBuf[%0d][%0d]", i, curBuf);
                doubleBufOut[i][curBuf].enq(entry);
            end
            else begin
                //$display("mkWLEngine reqQ->doubleBuf[%0d][%0d]", i, curBuf+1);
                doubleBufOut[i][curBuf+1].enq(entry);
                curBufOut[i] <= curBuf + 1;
            end
        endrule        
    end
    
    rule calcDone(started);
        // No readFSM.done since it's constantly triggered whenever any input buffers are empty!
        let cycle <- cur_cycle;
        Bool isDone = True;
        for(Integer i = 0; i < `WL_ENGINE_PORTS; i=i+1) begin
            if(reqQ[i].notEmpty || respQ[i].notEmpty || bufIn0[i].notEmpty || bufIn1[i].notEmpty || doubleBufOut[i][0].notEmpty || doubleBufOut[i][1].notEmpty) begin
                isDone = False;
            end
            //if(cycle == 10000000) $display("%0d: Lane %0d notEmpties: reqQ:%b respQ:%b bufIn0:%b bufIn1:%b, doubBufOut0:%b, doubBufOut1:%b, memReq:%b, memResp:%b", cur_cycle, i, reqQ[i].notEmpty, respQ[i].notEmpty, bufIn0[i].notEmpty, bufIn1[i].notEmpty, doubleBufOut[i][0].notEmpty, doubleBufOut[i][1].notEmpty,  memReqQ[i].notEmpty, memRespQ[i].notEmpty);
        end
        
        done <= (headPtr == tailPtr) && isDone;
        //if(cycle == 10000000) $display("%0d: wlEngine done calculation: HeadPtr:%0d, tailPtr:%0d, writeDone:%0b, readDone:%0b, isDone:%0b", cur_cycle, headPtr, tailPtr, writeFSM.done, readFSM.done, isDone);
    endrule
    
    method isDone();
        return done;
    endmethod
    
    method Action init(BC_AEId fpgaid, BC_Addr lockloc, BC_Addr headptrloc, BC_Addr tailptrloc, BC_Addr maxsize, BC_Addr bufferloc);
        $display("%0d: mkWLEngine[%0d]: INIT, fpgaid: %0x, lockLoc:%0x, headPtrLoc: %0x, tailPtrLoc: %0x, maxSize: %0d, bufferLoc: %0x", cur_cycle, fpgaid, fpgaid, lockloc, headptrloc, tailptrloc, maxsize, bufferloc);
        fpgaId <= fpgaid;
        lockLoc <= lockloc;
        headPtrLoc <= headptrloc;
        tailPtrLoc <= tailptrloc;
        maxSize <= maxsize;
        bufferLoc <= bufferloc;
        started <= True;
       
        writeFSM_curIdx <= 0;
        writeFSM_curBufIdx <= 0;
        triggerWriteFSM_timeout <= 0;
        
        for(Integer i = 0; i < `WL_ENGINE_PORTS; i=i+1) begin
            curBufIn[i] <= 0;
            curBufOut[i] <= 0;
        end
    endmethod
    
    interface streamIn = map(toPut, reqQ);
    interface streamOut = map(toGet, respQ);
    interface memReq = map(toGet, memReqQ);
    interface memResp = map(toPut, memRespQ);
endmodule


endpackage