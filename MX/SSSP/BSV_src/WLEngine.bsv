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

    interface Vector#(16, BC_MC_REQ) memReq;
    interface Vector#(16, BC_MC_RSP) memResp;

    method Action init(BC_AEId fpgaId, BC_Addr lockLoc, BC_Addr headPtrLoc, BC_Addr tailPtrLoc, BC_Addr maxSize, BC_Addr bufferLoc);
endinterface


`define WLENGINE_NUM_BUFS 2
`define WLENGINE_BUFOUT_SIZE 512
`define WLENTRY_SIZE 8
`define LG_WLENTRY_SIZE 3

module mkWLEngine(WLEngine);
    // memOffset[0]: lock bit
    // memOffset[1]: headPtr
    // memOffset[2]: tailPtr
    // memOffset[3]: workList size
    // memOffset[4]: start of data
    
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Reg#(BC_Addr) lockLoc <- mkRegU;
    Reg#(BC_Addr) headPtr <- mkRegU;
    Reg#(BC_Addr) headPtrLoc <- mkRegU;
    
    Reg#(BC_Addr) tailPtr <- mkRegU;
    Reg#(BC_Addr) tailPtrLoc <- mkRegU;
    
    Reg#(BC_Addr) maxSize <- mkRegU;
    Reg#(BC_Addr) bufferLoc <- mkRegU;
    
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) reqQ <- replicateM(mkFIFOF);
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) respQ <- replicateM(mkFIFOF);
    
    Vector#(16, FIFOF#(BC_MC_REQ)) memReqQ <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(BC_MC_RSP)) memRespQ <- replicateM(mkFIFOF);
    
    Vector#(`WL_ENGINE_PORTS, Vector#(2, FIFOF#(WLEntry))) doubleBufOut <- replicateM(replicateM(mkSizedBRAMFIFOF(`WLENGINE_BUFOUT_SIZE)));
    Vector#(`WL_ENGINE_PORTS, Reg#(Bit#(1))) curBufOut <- replicateM(mkReg(0));
    Vector#(32, PulseWire) fullBufOut <- replicateM(mkPulseWire);


    Reg#(Bit#(32)) readFSM_lockData <- mkReg(1);
    Reg#(BC_Addr) numReadEntries <- mkRegU;
    Reg#(BC_Addr) curReadEntry <- mkRegU;
    let readFSM <- mkFSM(
       seq
           // Obtain global worklist lock
           while(readFSM_lockData == 32'd1) action
               action
                   $display("%0d: mkWLEngine[%0d]: Reading lock bit...", cur_cycle, fpgaId);
                   memReqQ[0].enq(BC_MC_REQ{cmd_sub: REQ_ATOM_CAS, rtnctl: 1024, len: BC_4B, vadr: lockLoc, data: 64'h0000_0001});
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
           endaction
           
           // Get updated head and tail pointers
           action
               $display("%0d: mkWLEngine[%0d]: getting updated head/tail ptrs", cur_cycle, fpgaId);
               memReqQ[0].enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: 1024, len: BC_8B, vadr: headPtrLoc, data: ?});
               memReqQ[1].enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: 1024, len: BC_8B, vadr: tailPtrLoc, data: ?});
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
               BC_Addr size;
               if(headPtr < tailPtr) begin
                   size = tailPtr - headPtr;
               end
               else if(headPtr == tailPtr) begin
                   size = 0;
               end
               else if(headPtr > tailPtr) begin
                   size = (maxSize - tailPtr) + headPtr;
               end
               
               // Determine number of entries to read
               if(size > `WLENGINE_BUFOUT_SIZE) begin
                   numReadEntries <= `WLENGINE_BUFOUT_SIZE;
               end
               else begin
                   numReadEntries <= size;
               end
               
               curReadEntry <= 0;
           endaction
           
           while(curReadEntry < numReadEntries) action
               action
                   $display("Reading entry %0d of %0d...", curReadEntry, numReadEntries);
                   BC_Addr addr = bufferLoc + (headPtr << `LG_WLENTRY_SIZE);
                   memReqQ[0].enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: 1024, len: BC_8B, vadr: addr, data: ?});
                   
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
                   // Data is the old data, so if 1 then it failed
                   if(truncate(rsp.data) == 32'd1) begin
                       $display("  Someone currently using it, retry...");
                   end
                   
                   $display("  Data ", fshow(rsp));
               endaction
           endaction
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
        fpgaId <= fpgaId;
        lockLoc <= lockloc;
        headPtrLoc <= headptrloc;
        tailPtrLoc <= tailptrloc;
        maxSize <= maxsize;
        bufferLoc <= bufferloc;
    endmethod
    
    interface streamIn = map(toPut, reqQ);
    interface streamOut = map(toGet, respQ);
endmodule


endpackage