
package WorklistFIFO;

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

import Counter::*;

`define WL_ENGINE_PORTS 4
`define WL_SPILL_PORTS 2
`define WL_WLFIFO_SIZE 1024

typedef Bit#(32) WLPriority;
typedef Bit#(32) WLJob;
typedef Tuple2#(WLPriority, WLJob) WLEntry;
typedef Tuple2#(Bool, WLEntry) WLSpillReq; // True = Read, False = Write
typedef Tuple2#(Bool, WLEntry) WLSpillResp; // True = Read, False = Write

interface Worklist;
    interface Vector#(`WL_ENGINE_PORTS, Put#(WLEntry)) enq;
    interface Vector#(`WL_ENGINE_PORTS, Get#(WLEntry)) deq;
    
    interface Vector#(`WL_ENGINE_PORTS, Get#(WLSpillReq)) spillToMem;
    interface Vector#(`WL_ENGINE_PORTS, Put#(WLSpillResp)) memToSpill;
endinterface


(* synthesize *)
module mkWorklistFIFO(Worklist);
    
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) enqQ <- replicateM(mkFIFOF);
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) deqQ <- replicateM(mkFIFOF);
    
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLSpillReq)) spillToMemQ <- replicateM(mkFIFOF);
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLSpillResp)) memToSpillQ <- replicateM(mkFIFOF);
    
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) engineQ <- replicateM(mkSizedBRAMFIFOF(1024));
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) engineStagingQ <- replicateM(mkSizedFIFOF(2));
    
    // MAX engineSpillQ size is 255
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) engineSpillQ <- replicateM(mkSizedFIFOF(16));
    Vector#(`WL_ENGINE_PORTS, Reg#(Bit#(8)))  engineSpillCredits <- replicateM(mkReg(16));
    
    //FIFOF#(WLEntry) workQ <- mkSizedFIFOF(`WL_WLFIFO_SIZE);
    
    for(Integer i = 0; i < `WL_ENGINE_PORTS; i = i + 1) begin
        
        rule processIngress;
            WLEntry entry = enqQ[i].first();
            enqQ[i].deq();
            
            // If staging queue has room, put there. Else, put in local engine queue
            if(engineStagingQ[i].notFull()) begin
                engineStagingQ[i].enq(entry);
            end
            else begin
                engineQ[i].enq(entry);
            end
        endrule
        
        rule processEgress;
            WLEntry entry = ?;
            
            // If staging queue has entries, take from there to save bandwidth from local engine queue
            if(engineStagingQ[i].notEmpty()) begin
                entry = engineStagingQ[i].first();
                engineStagingQ[i].deq();
            end
            else if(engineQ[i].notEmpty()) begin
                entry = engineQ[i].first();
                engineQ[i].deq();
            end        
            // If no work, try to steal work from a neighbor
            else begin
                Integer stealIdx;
                if(i == 0) begin
                    stealIdx = `WL_ENGINE_PORTS-1;
                end
                else begin
                    stealIdx = i-1;
                end
                
                entry = engineQ[stealIdx].first();
                engineQ[stealIdx].deq();
            end
            
            deqQ[i].enq(entry);
        endrule
        
        
        rule processSpill;
            
            if(!engineQ[i].notEmpty) begin
                // Make read request. Reserve space for read first
                if(engineSpillCredits[i] > 0) begin
                    $display("Asking for data");
                    engineSpillCredits[i] <= engineSpillCredits[i] - 1;
                    spillToMemQ[i].enq(tuple2(True, ?));
                end
            end
            else if(!engineQ[i].notFull) begin
                // Make write request
                $display("Spilling data");
                WLEntry entry = engineQ[i].first();
                engineQ[i].deq();
                
                spillToMemQ[i].enq(tuple2(False, entry));
            end
        endrule
        
        rule processFill;
            WLSpillResp resp = memToSpillQ[i].first();
            memToSpillQ[i].deq();
            
            // If read response
            if(tpl_1(resp)) begin
                engineSpillQ[i].enq(tpl_2(resp));
            end
            else begin
                // If write response, just ignore
            end
        endrule
    end
    
    
    interface enq = map(toPut, enqQ);
    interface deq = map(toGet, deqQ);
    interface spillToMem = map(toGet, spillToMemQ);
    interface memToSpill = map(toPut, memToSpillQ);
endmodule

endpackage
