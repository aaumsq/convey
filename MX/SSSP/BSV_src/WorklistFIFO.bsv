
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
import BufBRAMFIFOF::*;
import GaloisTypes::*;
`include "GaloisDefs.bsv"

interface Worklist;
    interface Vector#(`WL_ENGINE_PORTS, Put#(WLEntry)) enq;
    interface Vector#(`WL_ENGINE_PORTS, Get#(WLEntry)) deq;
    
    interface Vector#(`WL_ENGINE_PORTS, Get#(BC_MC_REQ)) memReq;
    interface Vector#(`WL_ENGINE_PORTS, Put#(BC_MC_RSP)) memResp;
    
    method Action init(BC_AEId fpgaId, BC_Addr lockLoc, BC_Addr headPtrLoc, BC_Addr tailPtrLoc, BC_Addr maxSize, BC_Addr bufferLoc);
    method Bool isDone();
endinterface


(* synthesize *)
module mkWorklistFIFO(Worklist);
    
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) enqQs <- replicateM(mkFIFOF);
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) deqQs <- replicateM(mkFIFOF);
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) stealQs <- replicateM(mkFIFOF);
    Vector#(`WL_ENGINE_PORTS, Reg#(Bool)) reqSteals <- replicateM(mkReg(False));
    
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) engineQs <- replicateM(mkSizedBufBRAMFIFOF(1024));
    
    Reg#(Vector#(`WL_ENGINE_PORTS, Bool)) noEnqs <- mkReg(replicate(False));
    Reg#(Vector#(`WL_ENGINE_PORTS, Bool)) engineEmpties <- mkReg(replicate(False));
    Reg#(Bool) done <- mkRegU;
    
    WLEngine engine <- mkWLEngine();
    
    Vector#(`WL_ENGINE_PORTS, Wire#(Bool)) enqValid <- replicateM(mkDWire(False));

    rule calcDone;
        function Bool enqF(Integer x) = !enqValid[x];
        function Bool engineF(Integer x) = !engineQs[x].notEmpty;
        function Bool isTrue(Bool x) = x;
        noEnqs <= genWith(enqF);
        engineEmpties <= genWith(engineF);
        
        done <= all(isTrue, noEnqs) && all(isTrue, engineEmpties);
        //$display("noEnqs: %b, engineEmpties: %b", noEnqs, engineEmpties);
    endrule
    
    for(Integer i = 0; i < `WL_ENGINE_PORTS; i = i + 1) begin
        
        rule processFill;
            let pkt <- engine.streamOut[i].get;
            engineQs[i].enq(pkt);
            //$display("WorklistFIFO filling ",fshow(pkt));
        endrule
    
        rule processEnq;
            WLEntry pkt = enqQs[i].first;
            enqQs[i].deq();
            enqValid[i] <= True;
            
            Integer stealIdx = ?;
            if(i == `WL_ENGINE_PORTS-1)
                stealIdx = 0;
            else
                stealIdx = i + 1;
            
            if(reqSteals[stealIdx])
                stealQs[stealIdx].enq(pkt);
            else if(engineQs[i].notFull)
                engineQs[i].enq(pkt);
            else
                engine.streamIn[i].put(pkt);
        endrule
        
        rule processDeq(deqQs[i].notFull);
            if(engineQs[i].notEmpty) begin
                let pkt = engineQs[i].first();
                engineQs[i].deq();
                $display("Lane %0d deq from engineQ", i);
                deqQs[i].enq(pkt);
                reqSteals[i] <= False;
            end
            else if(stealQs[i].notEmpty) begin
                let pkt = stealQs[i].first();
                stealQs[i].deq();
                $display("Lane %0d deq from stealQ", i);
                deqQs[i].enq(pkt);
                reqSteals[i] <= False;
            end
            else begin
                reqSteals[i] <= True;
            end
        endrule
    end
    
    method Action init(BC_AEId fpgaid, BC_Addr lockloc, BC_Addr headptrloc, BC_Addr tailptrloc, BC_Addr maxsize, BC_Addr bufferloc);
        $display("%0d: mkWorklistFIFO[%0d]: INIT", cur_cycle, fpgaid);
        engine.init(fpgaid, lockloc, headptrloc, tailptrloc, maxsize, bufferloc);
        done <= False;
    endmethod
    
    method Bool isDone;
        return done;
    endmethod
    
    interface enq = map(toPut, enqQs);
    interface deq = map(toGet, deqQs);
    interface memReq = map(toGet, engine.memReq);
    interface memResp = map(toPut, engine.memResp);
endmodule

endpackage
