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


`include "GaloisTypes.bsv"

interface WLEngine;
    interface Vector#(`WL_ENGINE_PORTS, Put#(WLEntry)) streamIn;
    interface Vector#(`WL_ENGINE_PORTS, Reg#(Bool)) emptyReq;
    interface Vector#(`WL_ENGINE_PORTS, Get#(WLEntry)) streamOut;

    interface Vector#(16, Get#(BC_MC_REQ)) memReq;
    interface Vector#(16, Put#(BC_MC_RSP)) memResp;

    method Action init(BC_AEId fpgaId, BC_Addr lockLoc, BC_Addr headPtrLoc, BC_Addr tailPtrLoc, BC_Addr maxSize, BC_Addr bufferLoc);
endinterface


`define WLENGINE_NUM_BUFS 2
`define WLENGINE_BUFOUT_SIZE 512
`define WLENGINE_BUFIN_SIZE 512
`define WLENTRY_SIZE 8
`define LG_WLENTRY_SIZE 3
`define WLENGINE_BACKOFF 128

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
    
    Reg#(Bool) ready <- mkReg(False);
    
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) reqQ <- replicateM(mkFIFOF);
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) respQ <- replicateM(mkFIFOF);
    
    Vector#(16, FIFOF#(BC_MC_REQ)) memReqQ <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(BC_MC_RSP)) memRespQ <- replicateM(mkFIFOF);
    
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) bufIn <- replicateM(mkSizedBRAMFIFOF(`WLENGINE_BUFIN_SIZE));
    Vector#(`WL_ENGINE_PORTS, Vector#(2, FIFOF#(WLEntry))) doubleBufOut <- replicateM(replicateM(mkSizedBRAMFIFOF(`WLENGINE_BUFOUT_SIZE)));
    Vector#(`WL_ENGINE_PORTS, Reg#(Bit#(1))) curBufOut <- replicateM(mkReg(0));
    Vector#(32, PulseWire) fullBufOut <- replicateM(mkPulseWire);


    
    RWire#(Bit#(`WL_LG_ENGINE_PORTS)) bufInWriteIdxW <- mkRWire();
    
    rule setWires;
        function Bool bufInEmptyF(Integer x) = !bufIn[x].notEmpty;
        Vector#(`WL_ENGINE_PORTS, Bool) bufInEmpties = genWith(bufInEmptyF);
        let elem = findElem(True, bufInEmpties);
        if(isValid(elem)) begin
            Bit#(`WL_LG_ENGINE_PORTS) idx = pack(fromMaybe(?, elem));
            //$display("Empty idx: %0d", idx);
            bufInWriteIdxW.wset(idx);
        end
    endrule

    Reg#(Bit#(32)) readFSM_lockData <- mkRegU;
    Reg#(BC_Addr) readFSM_numEntries <- mkRegU;
    Reg#(BC_Addr) readFSM_curEntry <- mkRegU;
    Reg#(Bit#(32)) readFSM_backOff <- mkRegU;
    Reg#(Bit#(`WL_LG_ENGINE_PORTS)) readFSM_bufIdx <- mkRegU;
    
    let readFSM <- mkFSM(
       seq
           action
               readFSM_lockData <= 32'b1;
               readFSM_bufIdx <= fromMaybe(?, bufInWriteIdxW.wget());
               $display("%0d: mkWLEngine[%0d]: Starting readFSM, filling buf %0d...", cur_cycle, fpgaId, fromMaybe(?, bufInWriteIdxW.wget()));
           endaction
           
           // Obtain global worklist lock
           while(readFSM_lockData == 32'd1) seq
               action
                   $display("%0d: mkWLEngine[%0d]: Reading lock bit at addr: %0x...", cur_cycle, fpgaId, lockLoc);
                   memReqQ[0].enq(BC_MC_REQ{cmd_sub: REQ_ATOM_CAS, rtnctl: pack(GaloisAddress{mod:MK_WORKLIST, addr: ?}), len: BC_4B, vadr: lockLoc, data: 64'h0000_0001});
               endaction
           
               action
                   BC_MC_RSP rsp = memRespQ[0].first();
                   memRespQ[0].deq();
                   readFSM_lockData <= truncate(rsp.data);
                   $display("%0d: mkWLEngine[%0d]: old lock bit = %0d", cur_cycle, fpgaId, rsp.data);
                   // Data is the old data, so if 1 then it failed
                   if(truncate(rsp.data) == 32'd1) begin
                       $display("  Someone currently using it, retry...");
                   end
               endaction
          endseq
           
           // Get updated head and tail pointers
           action
               $display("%0d: mkWLEngine[%0d]: getting updated head/tail ptrs", cur_cycle, fpgaId);
               Bit#(32) gaddr = pack(GaloisAddress{mod: MK_WORKLIST, addr: ?});
               memReqQ[0].enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: gaddr, len: BC_8B, vadr: headPtrLoc, data: ?});
               memReqQ[1].enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: gaddr, len: BC_8B, vadr: tailPtrLoc, data: ?});
           endaction
           
           action
               BC_MC_RSP headRsp = memRespQ[0].first();
               BC_MC_RSP tailRsp = memRespQ[1].first();
               memRespQ[0].deq();
               memRespQ[1].deq();
               $display("%0d: mkWLEngine[%0d]: headPtr: %0x, tailPtr: %0x", cur_cycle, fpgaId, headRsp.data, tailRsp.data);
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
               if(size > `WLENGINE_BUFIN_SIZE) begin
                   entries = `WLENGINE_BUFIN_SIZE;
               end
               else begin
                   entries = size;
               end
               $display("%0d: mkWLEngine[%0d]: mkFSM WL size: %0d, reading %0d entries", cur_cycle, fpgaId, size, entries);
               readFSM_numEntries <= entries;
               readFSM_curEntry <= 0;
           endaction
           
           // Read entries and send to buffer
           if(readFSM_numEntries > 0) seq
               while(readFSM_curEntry < readFSM_numEntries) seq
                   action
                       $display("Reading entry %0d of %0d...", readFSM_curEntry, readFSM_numEntries);
                       BC_Addr addr = bufferLoc + (headPtr << `LG_WLENTRY_SIZE);
                       Bit#(32) gaddr = pack(GaloisAddress{mod: MK_WORKLIST, addr: ?});
                       
                       memReqQ[0].enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: gaddr, len: BC_8B, vadr: addr, data: ?});
                       
                       readFSM_curEntry <= readFSM_curEntry + 1;
                       
                       if(headPtr == (maxSize-1)) begin
                         headPtr <= 0;
                       end
                       else begin
                         headPtr <= headPtr + 1;
                       end
                   endaction
                   
                   action
                       BC_MC_RSP rsp = memRespQ[0].first();
                       memRespQ[0].deq();
                       
                       WLEntry entry = unpack(rsp.data);
                       $display("%0d: mkWLEngine[%0d]: entry priority: %0d, graphId: %0d", cur_cycle, fpgaId, tpl_1(entry), tpl_2(entry));
                       bufIn[readFSM_bufIdx].enq(entry);
                   endaction
               endseq
               
               // Write head and tailPtrs
               action
                   $display("%0d: mkWLEngine[%0d]: readFSM writing new head/tail ptrs", cur_cycle, fpgaId);
                   Bit#(32) gaddr = pack(GaloisAddress{mod: MK_WORKLIST, addr: ?});
                   memReqQ[0].enq(BC_MC_REQ{cmd_sub: REQ_WR, rtnctl: gaddr, len: BC_8B, vadr: headPtrLoc, data: extend(headPtr)});
                   memReqQ[1].enq(BC_MC_REQ{cmd_sub: REQ_WR, rtnctl: gaddr, len: BC_8B, vadr: tailPtrLoc, data: extend(tailPtr)});
               endaction
               
               // When head/tailPtr writes complete, unlock
               action
                   BC_MC_RSP headRsp = memRespQ[0].first();
                   BC_MC_RSP tailRsp = memRespQ[1].first();
                   memRespQ[0].deq();
                   memRespQ[1].deq();
                   memReqQ[0].enq(BC_MC_REQ{cmd_sub: REQ_WR, rtnctl: pack(GaloisAddress{mod:MK_WORKLIST, addr: ?}), len: BC_4B, vadr: lockLoc, data: 64'h0000_0000});
                   
                   $display("%0d: mkWLEngine[%0d]: readFSM done, unlocking!", cur_cycle, fpgaId);
               endaction
           endseq
           else seq
               // No data, unlock and stall avoid needless contention
              action
                   memReqQ[0].enq(BC_MC_REQ{cmd_sub: REQ_WR, rtnctl: pack(GaloisAddress{mod:MK_WORKLIST, addr: ?}), len: BC_4B, vadr: lockLoc, data: 64'h0000_0000});
                   
                   $display("%0d: mkWLEngine[%0d]: readFSM done, unlocking!", cur_cycle, fpgaId);
               endaction
               
               $display("%0d: mkWLEngine[%0d]: readFSM nothing to read, stalling for %0d cycles", cur_cycle, fpgaId, `WLENGINE_BACKOFF);
               readFSM_backOff <= 0;
               while(readFSM_backOff < `WLENGINE_BACKOFF) seq
                   action
                       readFSM_backOff <= readFSM_backOff + 1;
                   endaction
               endseq
           endseq           
       endseq
       );
    
    for(Integer i = 0; i < `WL_ENGINE_PORTS; i = i + 1) begin
        
        rule streamToBuf;
            WLEntry entry = reqQ[i].first();
            reqQ[i].deq();
            
            Bit#(1) curBuf = curBufOut[i];
            
            if(doubleBufOut[i][curBuf].notFull) begin
                doubleBufOut[i][curBuf].enq(entry);
            end
            else begin
                doubleBufOut[i][curBuf+1].enq(entry);
                curBufOut[i] <= curBuf + 1;
            end        
        endrule
        
        rule writeFullBufOut;
           if(!doubleBufOut[i][0].notFull)
               fullBufOut[i*2].send();
            if(!doubleBufOut[i][1].notFull)
                fullBufOut[i*2+1].send();
        endrule
        
        
    end
    
    method Action init(BC_AEId fpgaid, BC_Addr lockloc, BC_Addr headptrloc, BC_Addr tailptrloc, BC_Addr maxsize, BC_Addr bufferloc);
        $display("%0d: mkWLEngine[%0d]: INIT, fpgaid: %0x, lockLoc:%0x, headPtrLoc: %0x, tailPtrLoc: %0x, maxSize: %0d, bufferLoc: %0x", cur_cycle, fpgaid, fpgaid, lockloc, headptrloc, tailptrloc, maxsize, bufferloc);
        fpgaId <= fpgaid;
        lockLoc <= lockloc;
        headPtrLoc <= headptrloc;
        tailPtrLoc <= tailptrloc;
        maxSize <= maxsize;
        bufferLoc <= bufferloc;
        ready <= True;

        readFSM.start();
    endmethod
    
    interface streamIn = map(toPut, reqQ);
    interface streamOut = map(toGet, respQ);
    interface memReq = map(toGet, memReqQ);
    interface memResp = map(toPut, memRespQ);
endmodule


endpackage