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
import CoalescingCounter::*;
import GaloisTypes::*;
`include "GaloisDefs.bsv"

interface WLEngine;
    interface Vector#(`NUM_ENGINES, Put#(WLEntry)) streamIn;
    interface Vector#(`NUM_ENGINES, Get#(WLEntry)) streamOut;

    interface Vector#(16, Get#(MemReq)) memReq;
    interface Vector#(16, Put#(MemResp)) memResp;

    method Action init(BC_AEId fpgaId, BC_Addr lockLoc, BC_Addr headPtrLoc, BC_Addr tailPtrLoc, BC_Addr maxSize, BC_Addr bufferLoc);
    method Action stop();
    method Bool isDone();
endinterface


`define WLENGINE_BUFOUT_SIZE 1024
`define WLENGINE_BUFIN_SIZE 256
`define WLENTRY_SIZE 8
`define LG_WLENTRY_SIZE 3
`define WLENGINE_MAX_WRITES 16384
`define WLENGINE_BACKOFF 128
`define WRITEFSM_TIMEOUT 64
`define READFSM_TIMEOUT 2048

(* synthesize *)
module mkWLEngine(WLEngine);
    // memOffset[0]: lock bit
    // memOffset[1]: headPtr
    // memOffset[2]: tailPtr
    // memOffset[3]: workList size
    // memOffset[4]: start of data
    
    Reg#(BC_AEId) fpgaId <- mkReg(0);
    Reg#(BC_Addr) lockLoc <- mkRegU;
    Reg#(BC_Addr) lockLoc_r <- mkRegU;
    Reg#(BC_Addr) headPtr <- mkRegU;
    Reg#(BC_Addr) headPtr_w <- mkRegU;
    Reg#(BC_Addr) headPtrLoc <- mkRegU;
    
    Reg#(BC_Addr) tailPtr <- mkRegU;
    Reg#(BC_Addr) tailPtr_w <- mkRegU;
    Reg#(BC_Addr) tailPtrLoc <- mkRegU;
    
    Reg#(BC_Addr) maxSize <- mkRegU;
    Reg#(BC_Addr) bufferLoc <- mkRegU;
    
    Reg#(Bool) started <- mkReg(False);
    Reg#(Bool) done <- mkReg(False);
    
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) reqQ <- replicateM(mkFIFOF);
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) respQ <- replicateM(mkFIFOF);
    
    Vector#(16, FIFOF#(MemReq)) memReqQ <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemResp)) memRespQ <- replicateM(mkFIFOF);
    
    Vector#(`NUM_ENGINES, Vector#(2, FIFOF#(WLEntry))) doubleBufIn <- replicateM(replicateM(mkSizedBufBRAMFIFOF(`WLENGINE_BUFIN_SIZE)));
    Vector#(`NUM_ENGINES, Reg#(Bit#(1))) curBufIn <- replicateM(mkRegU);

    Vector#(`NUM_ENGINES, Vector#(2, FIFOF#(WLEntry))) doubleBufOut <- replicateM(replicateM(mkSizedBufBRAMFIFOF(`WLENGINE_BUFOUT_SIZE)));
    Vector#(`NUM_ENGINES, Reg#(Bit#(1))) curBufOut <- replicateM(mkRegU);

    
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
    
    Reg#(Bit#(32)) lock_lockData_w <- mkRegU;
    Reg#(Bit#(16)) lockFSM_backOff_w <- mkRegU;
    let lockFSM_w <- mkFSM(
       seq
           action
               lock_lockData_w <= 32'b1;
           endaction
           
           // Obtain global worklist lock
           while(lock_lockData_w == 32'd1) seq
               action
                   if(`DEBUG) $display("%0d: mkWLEngine[%0d]: Reading lock bit at addr: %0x...", cur_cycle, fpgaId, lockLoc);
                   memReqQ[0].enq(tagged MemCAS32{addr: lockLoc, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: ?}, cmpVal: 0, swapVal: 1});
               endaction
           
               action
                   MemResp rsp = memRespQ[0].first();
                   memRespQ[0].deq();
                   lock_lockData_w <= truncate(rsp.data);
                   if(`DEBUG) $display("%0d: mkWLEngine[%0d]: old lock bit = %0d", cur_cycle, fpgaId, rsp.data);
                   // Data is the old data, so if 1 then it failed
                   if(truncate(rsp.data) == 32'd1) begin
                       //if(`DEBUG) $display("%0d: mkWLEngine[%0d]:  Worklist is locked, retry...", cur_cycle, fpgaId);
                   end
               endaction
            endseq
       endseq
       );
    
    Reg#(Bit#(32)) lock_lockData <- mkRegU;
    Reg#(Bit#(16)) lockFSM_backOff <- mkRegU;
    let lockFSM <- mkFSM(
       seq
           action
               lock_lockData <= 32'b1;
           endaction
           
           // Obtain global worklist lock
           while(lock_lockData == 32'd1) seq
               action
                   if(`DEBUG) $display("%0d: mkWLEngine[%0d]: Reading lock bit at addr: %0x...", cur_cycle, fpgaId, lockLoc_r);
                   memReqQ[1].enq(tagged MemCAS32{addr: lockLoc_r, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: ?}, cmpVal: 0, swapVal: 1});
               endaction
           
               action
                   MemResp rsp = memRespQ[1].first();
                   memRespQ[1].deq();
                   lock_lockData <= truncate(rsp.data);
                   if(`DEBUG) $display("%0d: mkWLEngine[%0d]: old lock bit = %0d", cur_cycle, fpgaId, rsp.data);
                   // Data is the old data, so if 1 then it failed
                   if(truncate(rsp.data) == 32'd1) begin
                       //if(`DEBUG) $display("%0d: mkWLEngine[%0d]:  Worklist is locked, retry...", cur_cycle, fpgaId);
                   end
               endaction
/*
               if(lock_lockData == 32'd1) seq
                   if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM nothing to read, stalling for %0d cycles", cur_cycle, fpgaId, `WLENGINE_BACKOFF);
                   lockFSM_backOff <= 0;
                   while(lockFSM_backOff < `WLENGINE_BACKOFF) seq
                       action
                         lockFSM_backOff <= lockFSM_backOff + 1;
                       endaction
                   endseq
               endseq
*/
          endseq
           
       endseq
       );

    // Write FSM: Writes all full buffers
    FIFOF#(Bit#(5)) writeFSM_writeQ <- mkSizedFIFOF(16);
    Reg#(Bit#(`LG_NUM_ENGINES)) writeFSM_curIdx <- mkRegU;
    Reg#(Bit#(1)) writeFSM_curBufIdx <- mkRegU;
    Reg#(Bit#(16)) writeFSM_totalWrites <- mkRegU;
    Reg#(Bool) writeFSM_done <- mkRegU;
    Reg#(BC_Addr) writeFSM_wlSize <- mkRegU;
    Reg#(BC_Addr) writeFSM_maxSizeMinusOne <- mkRegU;
    FIFOF#(Bit#(0)) writeFSM_outstandingWrites <- mkSizedFIFOF(256);
    
    rule writeFSM_catchWriteAcks(writeFSM_outstandingWrites.notEmpty);
        memRespQ[4].deq();
        writeFSM_outstandingWrites.deq();
    endrule
    
    let writeFSM <- mkFSM(
       seq
           action
               if(`DEBUG) $display("%0d: mkWLEngine: WriteFSM START", cur_cycle);
               lockFSM_w.start();
               writeFSM_curIdx <= 0;
               writeFSM_totalWrites <= 0;
               writeFSM_done <= False;
           endaction
           
           action
               if(`DEBUG) $display("%0d: mkWLEngine: WriteFSM lock + head/tailPtr obtained! headPtr=%0d, tailPtr=%0d", cur_cycle, headPtr, tailPtr_w);
               lockFSM_w.waitTillDone();
           endaction

           // Get updated head and tail pointers
           action
               //$display("%0d: mkWLEngine[%0d]: getting updated head/tail ptrs", cur_cycle, fpgaId);
               GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: ?};
               memReqQ[4].enq(tagged MemRead64{addr: headPtrLoc, gaddr: gaddr});
               memReqQ[5].enq(tagged MemRead64{addr: tailPtrLoc, gaddr: gaddr});
           endaction
           
           action
               MemResp headRsp = memRespQ[4].first();
               MemResp tailRsp = memRespQ[5].first();
               memRespQ[4].deq();
               memRespQ[5].deq();
               if(`DEBUG) $display("%0d: mkWLEngine[%0d]: headPtr: %0x, tailPtr: %0x", cur_cycle, fpgaId, headRsp.data, tailRsp.data);
               headPtr_w <= truncate(pack(headRsp.data));
               tailPtr_w <= truncate(pack(tailRsp.data));
           endaction

           action
               writeFSM_wlSize <= getWLSize(headPtr_w, tailPtr_w, maxSize);
               writeFSM_maxSizeMinusOne <= maxSize-1;
           endaction
           
           while(!writeFSM_done) seq
               action
                   if(`DEBUG) $display("%0d: mkWLEngine WriteFSM checking doubleBufOut[%0d][%0d], writeFSM_done: %0d, notEmpty: %0d", cur_cycle, writeFSM_curIdx, writeFSM_curBufIdx, writeFSM_done, doubleBufOut[writeFSM_curIdx][writeFSM_curBufIdx].notEmpty);
                   if(writeFSM_totalWrites > `WLENGINE_MAX_WRITES) begin
                       writeFSM_done <= True;
                   end
                   else if(doubleBufOut[writeFSM_curIdx][writeFSM_curBufIdx].notEmpty) begin
                       if(writeFSM_wlSize < writeFSM_maxSizeMinusOne) begin
                           WLEntry entry = doubleBufOut[writeFSM_curIdx][writeFSM_curBufIdx].first();
                           doubleBufOut[writeFSM_curIdx][writeFSM_curBufIdx].deq();
                          
                           BC_Addr addr = bufferLoc + (tailPtr_w << `LG_WLENTRY_SIZE);
                           memReqQ[4].enq(tagged MemWrite64{addr: addr, gaddr: GaloisAddress{mod: MK_WORKLIST, addr: ?}, data: pack(entry)});
                           writeFSM_outstandingWrites.enq(?);
                           writeFSM_totalWrites <= writeFSM_totalWrites + 1;
                          
                           if(`DEBUG) $display("mkWLEngine: WriteFSM headPtr=%0d, tailPtr=%0d, packet: %x", headPtr_w, tailPtr_w, entry);
                           if(tailPtr_w == (maxSize-1)) begin
                             tailPtr_w <= 0;
                           end
                           else begin
                             tailPtr_w <= tailPtr_w + 1;
                           end
                           writeFSM_wlSize <= writeFSM_wlSize + 1;
                       end
                       else begin
                           if(`DEBUG) $display("mkWLEngine WorkList is FULL! Breaking...");
                           writeFSM_done <= True;
                       end
                   end
                   else begin
                       if(`DEBUG) $display("mkWLEngine WriteFSM done with idx %0d", writeFSM_curIdx);
                       if(writeFSM_curIdx != fromInteger(`NUM_ENGINES-1)) begin
                           writeFSM_curIdx <= writeFSM_curIdx + 1;
                           if(`DEBUG) $display("mkWLEngine WriteFSM incrementing writeFSM_curIdx: %0d", writeFSM_curIdx+1);
                       end
                       else begin
                           if(`DEBUG) $display("%0d: mkWLEngine: WriteFSM writeFSM done!", cur_cycle);
                           writeFSM_done <= True;
                       end
                   end
               endaction
           endseq
           
           while(writeFSM_outstandingWrites.notEmpty) seq
               action
                   //$display("%0d: WriteFSM: waiting for %0d writes to complete", cur_cycle, writeFSM_numWrites.getVal());
                   noAction;
               endaction
           endseq
           
           // Write head and tailPtrs
           action
               if(`DEBUG) $display("%0d: mkWLEngine[%0d]: readFSM writing new head/tail ptrs, headPtr=%0d, tailPtr=%0d", cur_cycle, fpgaId, headPtr_w, tailPtr_w);
               GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: ?};
               //memReqQ[4].enq(tagged MemWrite64{addr: headPtrLoc, gaddr: gaddr, data: extend(headPtr_w)});
               memReqQ[5].enq(tagged MemWrite64{addr: tailPtrLoc, gaddr: gaddr, data: extend(tailPtr_w)});
           endaction
           
           // When head/tailPtr writes complete, unlock
           action
               //memRespQ[4].deq();
               memRespQ[5].deq();
               memReqQ[4].enq(tagged MemWrite32{addr: lockLoc, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: ?}, data: 0});
               writeFSM_curBufIdx <= writeFSM_curBufIdx + 1;
           endaction
           
           action
               if(writeFSM_totalWrites > 0) 
                   $display("%0d: mkWLEngine[%0d]: WriteFSM wrote %0d entries", cur_cycle, fpgaId, writeFSM_totalWrites);
               memRespQ[4].deq();
               if(`DEBUG) $display("%0d: mkWLEngine[%0d]: WriteFSM done, unlocking!", cur_cycle, fpgaId);
           endaction
       endseq
       );
    
    // Read FSM: Fills one buffer
    Reg#(BC_Addr) readFSM_numEntries <- mkRegU;
    Vector#(`NUM_ENGINES, Reg#(BC_Addr)) readFSM_curEntry <- replicateM(mkRegU);
    Reg#(Bit#(10)) readFSM_backOff <- mkRegU;
    Reg#(Bit#(`LG_NUM_ENGINES)) readFSM_bufIdx <- mkRegU;
    Reg#(Bit#(1)) readFSM_buf <- mkRegU;
    Reg#(Bool) readFSM_success <- mkRegU;
    Vector#(`NUM_ENGINES, FIFOF#(Bit#(0))) readFSM_outstandingReads <- replicateM(mkSizedFIFOF(256));
    Reg#(BC_Addr) headPtr_r <- mkRegU;
    Reg#(BC_Addr) tailPtr_r <- mkRegU;
    
    for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
        rule readFSM_processReads(readFSM_outstandingReads[i].notEmpty);
            MemResp rsp = memRespQ[2+i*4].first();
            memRespQ[2+i*4].deq();
            readFSM_outstandingReads[i].deq();
            
            WLEntry entry = unpack(rsp.data);
            if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM entry priority: %0x, graphId: %0x", cur_cycle, fpgaId, tpl_1(entry), tpl_2(entry));
            doubleBufIn[i][readFSM_buf].enq(entry);
        endrule
    end
    
    let readFSM <- mkFSM(
        seq
            action
                //$display("%0d: mkWLEngine[%0d]: getting updated head/tail ptrs", cur_cycle, fpgaId);
                GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: ?};
                memReqQ[8].enq(tagged MemRead64{addr: headPtrLoc, gaddr: gaddr});
                memReqQ[12].enq(tagged MemRead64{addr: tailPtrLoc, gaddr: gaddr});
            endaction
            
            action
                MemResp headRsp = memRespQ[8].first();
                MemResp tailRsp = memRespQ[12].first();
                memRespQ[8].deq();
                memRespQ[12].deq();
                if(`DEBUG) $display("%0d: mkWLEngine[%0d]: headPtr: %0x, tailPtr: %0x", cur_cycle, fpgaId, headRsp.data, tailRsp.data);
                headPtr_r <= truncate(pack(headRsp.data));
                tailPtr_r <= truncate(pack(tailRsp.data));
            endaction

            if (headPtr_r != tailPtr_r) seq
                action
                    readFSM_success <= False;
                    lockFSM.start();
                    if(`DEBUG) $display("%0d: mkWLEngine[%0d]: Starting readFSM, filling buf %0d idx %0d...", cur_cycle, fpgaId, readFSM_buf, readFSM_bufIdx);
                endaction
                
                action
                    lockFSM.waitTillDone();
                endaction

                // Get updated head and tail pointers
                action
                    //$display("%0d: mkWLEngine[%0d]: getting updated head/tail ptrs", cur_cycle, fpgaId);
                    GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: ?};
                    memReqQ[8].enq(tagged MemRead64{addr: headPtrLoc, gaddr: gaddr});
                    memReqQ[12].enq(tagged MemRead64{addr: tailPtrLoc, gaddr: gaddr});
                endaction
                
                action
                    MemResp headRsp = memRespQ[8].first();
                    MemResp tailRsp = memRespQ[12].first();
                    memRespQ[8].deq();
                    memRespQ[12].deq();
                    if(`DEBUG) $display("%0d: mkWLEngine[%0d]: headPtr: %0x, tailPtr: %0x", cur_cycle, fpgaId, headRsp.data, tailRsp.data);
                    headPtr <= truncate(pack(headRsp.data));
                    tailPtr <= truncate(pack(tailRsp.data));
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
                    if(size > (`NUM_ENGINES*`WLENGINE_BUFIN_SIZE)) begin
                        entries = (`NUM_ENGINES*`WLENGINE_BUFIN_SIZE);
                    end
                    else begin
                        entries = size;
                    end
                    $display("%0d: mkWLEngine[%0d]: ReadFSM headPtr: %0d, tailPtr: %0d, WL size: %0d, reading %0d entries", cur_cycle, fpgaId, headPtr, tailPtr, size, entries);
                    readFSM_numEntries <= entries;
         	   for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
                        readFSM_curEntry[i] <= fromInteger(i);
         	   end
                endaction
                
                // Read entries and send to buffer
                if(readFSM_numEntries > 0) seq
                    while(readFSM_curEntry[`NUM_ENGINES-1] < readFSM_numEntries) seq
                        action
                            //if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM Reading entry %0d of %0d, writing to buf%0d[%0d]...", cur_cycle, fpgaId, readFSM_curEntry, readFSM_numEntries, readFSM_bufIdx, readFSM_buf);
         		    for (Integer i = 0; i < `NUM_ENGINES; i = i+1) begin
                                 BC_Addr addr = bufferLoc + ((headPtr+fromInteger(i)) << `LG_WLENTRY_SIZE);
                                 memReqQ[2+i*4].enq(tagged MemRead64{addr: addr, gaddr: GaloisAddress{mod: MK_WORKLIST, addr: ?}});
                                 readFSM_outstandingReads[i].enq(?);
                                 
                                 readFSM_curEntry[i] <= readFSM_curEntry[i] + `NUM_ENGINES;
         		    end
                            
                            if(headPtr == (maxSize-`NUM_ENGINES)) begin
                                headPtr <= 0;
                            end
                            else begin
                                headPtr <= headPtr + `NUM_ENGINES;
                            end
                        endaction
                    endseq

         	    action
         	        UInt#(2) inc = 0;
         	        for (Integer i = 0; i < `NUM_ENGINES-1; i = i+1) begin
         	            if (readFSM_curEntry[i] < readFSM_numEntries) begin
                                  BC_Addr addr = bufferLoc + ((headPtr+fromInteger(i)) << `LG_WLENTRY_SIZE);
                                  memReqQ[2+i*4].enq(tagged MemRead64{addr: addr, gaddr: GaloisAddress{mod: MK_WORKLIST, addr: ?}});
                                  readFSM_outstandingReads[i].enq(?);
         	         	inc = inc + 1;
         	            end 
         	        end
         	        BC_Addr newHeadPtr = headPtr + extend(pack(inc));
         	        if (newHeadPtr > maxSize) begin
         	            headPtr <= newHeadPtr - maxSize;
         	        end
         	        else begin
         	            headPtr <= newHeadPtr;
         	        end
         	    endaction
                    
                    while(readFSM_outstandingReads[0].notEmpty || readFSM_outstandingReads[1].notEmpty || readFSM_outstandingReads[2].notEmpty || readFSM_outstandingReads[3].notEmpty) seq
                        action
                            //$display("%0d: ReadFSM: waiting for %0d reads to complete", cur_cycle, readFSM_numReads.getVal());
                            noAction;
                        endaction
                    endseq
                    
                    // Write head and tailPtrs
                    action
                        $display("%0d: mkWLEngine[%0d]: ReadFSM writing new head/tail ptrs, headPtr=%0d, tailPtr=%0d", cur_cycle, fpgaId, headPtr, tailPtr);
                        GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: ?};
                        memReqQ[2].enq(tagged MemWrite64{addr: headPtrLoc, gaddr: gaddr, data: extend(headPtr)});
                        //memReqQ[3].enq(tagged MemWrite64{addr: tailPtrLoc, gaddr: gaddr, data: extend(tailPtr)});
                    endaction
                    
                    // When head/tailPtr writes complete, unlock
                    action
                        memRespQ[2].deq();
                        //memRespQ[3].deq();
                        memReqQ[2].enq(tagged MemWrite32{addr: lockLoc_r, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: ?}, data: 0});
                    endaction
                    
                    action
                        if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM done, unlocking!", cur_cycle, fpgaId);
                        if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM read %0d entries for idx %0d[%0d]", cur_cycle, fpgaId, readFSM_numEntries, readFSM_bufIdx, readFSM_buf);
                        memRespQ[2].deq();
                        readFSM_success <= True;
                    endaction
                endseq
                else seq
                    // No data, unlock and stall avoid needless contention
                    action
                        memReqQ[2].enq(tagged MemWrite32{addr: lockLoc_r, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: ?}, data: 0});
                        if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM done, unlocking!", cur_cycle, fpgaId);
                    endaction
                    
                    action
                        memRespQ[2].deq();
                    endaction
/*                    
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
 */
                endseq
            endseq
            else seq
                action
                    readFSM_backOff <= 0;
                    if (`DEBUG) $display("%0d: mkWLEngine[%d]: readFSM nothing to read", cur_cycle, fpgaId);
                endaction

                while (readFSM_backOff < `WLENGINE_BACKOFF) seq
                    action 
                        readFSM_backOff <= readFSM_backOff + 1;
                    endaction
                endseq
            endseq
        endseq
    );
    
    rule startRead(started && readFSM.done);
        function Bool bufInEmptyF0(Integer x) = !doubleBufIn[x][0].notEmpty;
        function Bool bufInEmptyF1(Integer x) = !doubleBufIn[x][1].notEmpty;
        function Bool isTrueF(Bool x) = x;
        Vector#(`NUM_ENGINES, Bool) bufInEmpties0 = genWith(bufInEmptyF0);
        Vector#(`NUM_ENGINES, Bool) bufInEmpties1 = genWith(bufInEmptyF1);
        //let elem0 = findElem(True, bufInEmpties0);
        //let elem1 = findElem(True, bufInEmpties1);
	let empty0 = all(isTrueF, bufInEmpties0);
	let empty1 = all(isTrueF, bufInEmpties1);
        //if(isValid(elem0)) begin
        if(empty0) begin
            //Bit#(`LG_NUM_ENGINES) idx = pack(fromMaybe(?, elem0));
            //$display("BufInEmpties: %b, Empty idx: %0d", bufInEmpties0, idx);
            //readFSM_bufIdx <= idx;
            readFSM_buf <= 0;
            readFSM.start();
        end
        else if(empty1) begin
            //Bit#(`LG_NUM_ENGINES) idx = pack(fromMaybe(?, elem1));
            //$display("BufInEmpties: %b, Empty idx: %0d", idx);
            //readFSM_bufIdx <= idx;
            readFSM_buf <= 1;
            readFSM.start();
        end
    endrule
    
    
    Reg#(Bit#(16)) triggerWriteFSM_timeout <- mkRegU;
    Reg#(Bit#(1)) triggerWriteFSM_lastIdx <- mkRegU;
    rule triggerWriteFSM(started && writeFSM.done);
        function Bool bufOutFullF(Integer x) = !doubleBufOut[x][writeFSM_curBufIdx].notFull;
        function Bool bufOutEmptyF(Integer x) = doubleBufOut[x][writeFSM_curBufIdx].notEmpty;
        Vector#(`NUM_ENGINES, Bool) bufOutFulls = genWith(bufOutFullF);
        Vector#(`NUM_ENGINES, Bool) bufOutEmpties = genWith(bufOutEmptyF);
        function Bool isTrueF(Bool x) = x;
        if(any(isTrueF, bufOutFulls)) begin
            //$display("WLEngine triggerWriteFSM EngineQs[%0d] full: %0b", writeFSM_curIdx, bufOutFulls);
            writeFSM.start();
            triggerWriteFSM_timeout <= 0;
        end
        else begin
            //$display("WLEngine triggerWriteFSM curBuf: %0d full: %b", writeFSM_curBufIdx, bufOutFulls);
            if(triggerWriteFSM_timeout > `WRITEFSM_TIMEOUT && any(isTrueF, bufOutEmpties)) begin
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
    
    
    for(Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
        
        rule setCurBufIn(started);
            if(((curBufIn[i] == 0) && !doubleBufIn[i][0].notEmpty && doubleBufIn[i][1].notEmpty) ||
               ((curBufIn[i] == 1) && !doubleBufIn[i][1].notEmpty && doubleBufIn[i][0].notEmpty)) begin
               // Flip bit
               curBufIn[i] <= 1 + curBufIn[i];
            end
            
            if((curBufIn[i] == 0) && doubleBufIn[i][0].notEmpty) begin
                WLEntry pkt = doubleBufIn[i][0].first();
                if(`DEBUG) $display("%0d: mkWLEngine curBufIn[0] packet %x streaming to WorkListFIFOF", cur_cycle, pkt);
                doubleBufIn[i][0].deq();
                respQ[i].enq(pkt);
            end
            if((curBufIn[i] == 1) && doubleBufIn[i][1].notEmpty) begin
                WLEntry pkt = doubleBufIn[i][1].first();
                if(`DEBUG) $display("%0d: mkWLEngine curBufIn[1] packet %x streaming to WorkListFIFOF", cur_cycle, pkt);
                doubleBufIn[i][1].deq();
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
        for(Integer i = 0; i < `NUM_ENGINES; i=i+1) begin
            if(reqQ[i].notEmpty || respQ[i].notEmpty || doubleBufIn[i][0].notEmpty || doubleBufIn[i][1].notEmpty || doubleBufOut[i][0].notEmpty || doubleBufOut[i][1].notEmpty || readFSM_outstandingReads[i].notEmpty) begin
                isDone = False;
            end
            if(cycle % 1024 == 0) $display("%0d: Lane[%0d][%0d] notEmpties: reqQ:%b respQ:%b doubleBufIn0:%b doubleBufIn1:%b, doubBufOut0:%b, doubBufOut1:%b, memReq:%b, memResp:%b", cur_cycle, fpgaId, i, reqQ[i].notEmpty, respQ[i].notEmpty, doubleBufIn[i][0].notEmpty, doubleBufIn[i][1].notEmpty, doubleBufOut[i][0].notEmpty, doubleBufOut[i][1].notEmpty,  memReqQ[i].notEmpty, memRespQ[i].notEmpty);
        end
        
        done <= (headPtr_r == tailPtr_r) && (headPtr == tailPtr) && isDone && (!writeFSM_outstandingWrites.notEmpty);
	//$display("WLEngine[%0d]: ptr done is %0d, empty done is %0d", fpgaId, (headPtr == tailPtr), isDone);
        //if(cycle == 10000000) $display("%0d: wlEngine done calculation: HeadPtr:%0d, tailPtr:%0d, writeDone:%0b, readDone:%0b, isDone:%0b", cur_cycle, headPtr, tailPtr, writeFSM.done, readFSM.done, isDone);
    endrule
    
    method isDone();
        return done;
    endmethod
    
    method Action init(BC_AEId fpgaid, BC_Addr lockloc, BC_Addr headptrloc, BC_Addr tailptrloc, BC_Addr maxsize, BC_Addr bufferloc);
        $display("%0d: mkWLEngine[%0d]: INIT, fpgaid: %0x, lockLoc:%0x, headPtrLoc: %0x, tailPtrLoc: %0x, maxSize: %0d, bufferLoc: %0x", cur_cycle, fpgaid, fpgaid, lockloc, headptrloc, tailptrloc, maxsize, bufferloc);
        fpgaId <= fpgaid;
        lockLoc <= lockloc;
	lockLoc_r <= lockloc + 8;
        headPtrLoc <= headptrloc;
        tailPtrLoc <= tailptrloc;
	headPtr <= 0;
	tailPtr <= 1;
        maxSize <= maxsize;
        bufferLoc <= bufferloc;
        started <= True;
        
        
        writeFSM_curIdx <= 0;
        writeFSM_curBufIdx <= 0;
        triggerWriteFSM_timeout <= 0;
        
        for(Integer i = 0; i < `NUM_ENGINES; i=i+1) begin
            curBufIn[i] <= 0;
            curBufOut[i] <= 0;
        end
    endmethod

    method Action stop;
        started <= False;
    endmethod
    
    interface streamIn = map(toPut, reqQ);
    interface streamOut = map(toGet, respQ);
    interface memReq = map(toGet, memReqQ);
    interface memResp = map(toPut, memRespQ);
endmodule


endpackage
