
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

//import CoalescingCounter::*;
import WLEngine::*;

`include "GaloisTypes.bsv"

interface Worklist;
    interface Vector#(`WL_ENGINE_PORTS, Put#(WLEntry)) enq;
    interface Vector#(`WL_ENGINE_PORTS, Get#(WLEntry)) deq;
    
    interface Vector#(`WL_ENGINE_PORTS, Get#(BC_MC_REQ)) memReq;
    interface Vector#(`WL_ENGINE_PORTS, Put#(BC_MC_RSP)) memResp;
    
    method Action init(BC_AEId fpgaId, BC_Addr lockLoc, BC_Addr headPtrLoc, BC_Addr tailPtrLoc, BC_Addr maxSize, BC_Addr bufferLoc);
    
endinterface


(* synthesize *)
module mkWorklistFIFO(Worklist);
    
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) enqQ <- replicateM(mkFIFOF);
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) deqQ <- replicateM(mkFIFOF);
    
    Vector#(`WL_ENGINE_PORTS, FIFOF#(BC_MC_REQ)) spillToMemQ <- replicateM(mkFIFOF);
    Vector#(`WL_ENGINE_PORTS, FIFOF#(BC_MC_RSP)) memToSpillQ <- replicateM(mkFIFOF);
    
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) engineQ <- replicateM(mkSizedBRAMFIFOF(1024));
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) engineStagingQ <- replicateM(mkSizedFIFOF(2));
    
    // MAX engineSpillQ size is 255
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) engineSpillQ <- replicateM(mkSizedFIFOF(16));
    Vector#(`WL_ENGINE_PORTS, Reg#(Bit#(8)))  engineSpillCredits <- replicateM(mkReg(16));
    
    Reg#(BC_Addr) spillOffset <- mkRegU;
    Reg#(BC_Addr) headPtr <- mkRegU;
    Reg#(BC_Addr) tailPtr <- mkRegU;
    Reg#(BC_Addr) totalSize <- mkRegU;
    
    WLEngine engine <- mkWLEngine();
    
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
        
        /*
        rule processSpill;
            
            if(!engineQ[i].notEmpty) begin
                // Make read request. Reserve space for read first
                if(engineSpillCredits[i] > 0) begin
                    $display("Asking for data");
                    engineSpillCredits[i] <= engineSpillCredits[i] - 1;
                    
                    let req = BC_MC_REQ {cmd_sub: REQ_RD, rtnctl: 0, len: BC_8B, vadr: headPtr, data: ?};
                    //spillToMemQ[i].enq(req);
                end
            end
            else if(!engineQ[i].notFull) begin
                // Make write request
                $display("Spilling data");
                WLEntry entry = engineQ[i].first();
                engineQ[i].deq();
                
                //spillToMemQ[i].enq(tuple2(False, entry));
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
        endrule */
    end
        
    method Action init(BC_AEId fpgaid, BC_Addr lockloc, BC_Addr headptrloc, BC_Addr tailptrloc, BC_Addr maxsize, BC_Addr bufferloc);
        $display("%0d: mkWorklistFIFO[%0d]: INIT", cur_cycle, fpgaid);
        engine.init(fpgaid, lockloc, headptrloc, tailptrloc, maxsize, bufferloc);
    endmethod
    
    interface enq = map(toPut, enqQ);
    interface deq = map(toGet, deqQ);
    interface memReq = map(toGet, engine.memReq);
    interface memResp = map(toPut, engine.memResp);
endmodule

endpackage
