
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
import WireFIFOF::*;
import GaloisTypes::*;
`include "GaloisDefs.bsv"

interface Worklist;
    interface Vector#(`NUM_ENGINES, Put#(WLEntry)) enq;
    interface Vector#(`NUM_ENGINES, Get#(WLEntry)) deq;
    
    interface Vector#(16, Get#(MemReq)) memReq;
    interface Vector#(16, Put#(MemResp)) memResp;
    
    method Action init(BC_AEId fpgaId, BC_Addr lockLoc, BC_Addr headPtrLoc, BC_Addr tailPtrLoc, BC_Addr maxSize, BC_Addr bufferLoc);
    method Bool isDone();
endinterface


(* synthesize *)
module mkWorklistFIFO(Worklist);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) enqQs <- replicateM(mkFIFOF);
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) deqQs <- replicateM(mkFIFOF);
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) stealQs <- replicateM(mkSizedWireFIFOF(2));
    Vector#(`NUM_ENGINES, Reg#(Bool)) reqSteals <- replicateM(mkRegU);
    
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) engineQs <- replicateM(mkSizedBufBRAMFIFOF(1024));
    
    Reg#(Vector#(`NUM_ENGINES, Bool)) noEnqs <- mkRegU;
    Reg#(Vector#(`NUM_ENGINES, Bool)) engineEmpties <- mkRegU;
    Reg#(Bool) done <- mkReg(False);
    Reg#(Bool) started <- mkReg(False);
    
    WLEngine engine <- mkWLEngine();
    
    Vector#(`NUM_ENGINES, Wire#(Bool)) enqValid <- replicateM(mkDWire(False));

    rule calcDone(started);
        function Bool enqF(Integer x) = !enqValid[x];
        function Bool engineF(Integer x) = !engineQs[x].notEmpty;
        function Bool isTrue(Bool x) = x;
        noEnqs <= genWith(enqF);
        engineEmpties <= genWith(engineF);
        
        done <= all(isTrue, noEnqs) && all(isTrue, engineEmpties);
    endrule
    
    rule stealReqs(started);
        function Bool isTrue(Bool x) = x;
        function Bool stealsF(Integer x) = reqSteals[x];
        function Bool engineF(Integer x) = engineQs[x].notEmpty;
        Vector#(`NUM_ENGINES, Bool) steals = genWith(stealsF);
        Vector#(`NUM_ENGINES, Bool) engines = genWith(engineF);
        if(any(isTrue, steals)) begin
            if(`DEBUG) $display("steals: %b, engineQs: %b", steals, engines);
        end
    endrule
    
    for(Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
        (* descending_urgency = "processEnq, processFill, engineFull" *)
        rule engineFull(started);
            if(!engineQs[i].notFull) begin
                if(`DEBUG) $display("WorklistFIFO engineQ[%0d] is full!", i);
            end
            let cycle <- cur_cycle;
            //if(cycle == 10000000) $display("WorklistFIFO[%0d][%0d] empties: enqQs: %b, deqQs: %b, stealQs: %b, reqSteals (reg): %b, engineQs: %b", fpgaId, i, !enqQs[i].notEmpty, !deqQs[i].notEmpty, !stealQs[i].notEmpty, reqSteals[i], !engineQs[i].notEmpty);
            //if(cycle == 10000000) $display("WorklistFIFO[%0d][%0d] fulls: enqQs: %b, deqQs: %b, stealQs: %b, reqSteals (reg): %b, engineQs: %b", fpgaId, i, !enqQs[i].notFull, !deqQs[i].notFull, !stealQs[i].notFull, reqSteals[i], !engineQs[i].notFull);
        endrule
        
        rule processFill(started);
            let pkt <- engine.streamOut[i].get;
            engineQs[i].enq(pkt);
            if(`DEBUG) $display("WorklistFIFO filling engineQ[%0d] ", i, fshow(pkt));
        endrule
        
        rule processEnq(started);
            WLEntry pkt = enqQs[i].first;
            enqQs[i].deq();
            enqValid[i] <= True;
            Integer stealIdx = ?;
            if(i == `NUM_ENGINES-1)
                stealIdx = 0;
            else
                stealIdx = i + 1;
            
            if(reqSteals[stealIdx] && stealQs[stealIdx].notFull)
                stealQs[stealIdx].enq(pkt);
            else if(engineQs[i].notFull) begin
                if(`DEBUG) $display("WorklistFIFO enqing engineQ[%0d] ", i, fshow(pkt));
                engineQs[i].enq(pkt);
            end
            else
                engine.streamIn[i].put(pkt);
        endrule
        
        rule processDeq(started && deqQs[i].notFull);
            if(engineQs[i].notEmpty) begin
                let pkt = engineQs[i].first();
                engineQs[i].deq();
                if(`DEBUG) $display("Lane %0d deq from engineQ", i);
                deqQs[i].enq(pkt);
                reqSteals[i] <= False;
            end
            else if(stealQs[i].notEmpty) begin
                let pkt = stealQs[i].first();
                stealQs[i].deq();
                if(`DEBUG) $display("Lane %0d deq from stealQ", i);
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
        started <= True;
        fpgaId <= fpgaid;
        
        for(Integer i = 0; i < `NUM_ENGINES; i=i+1) begin
            reqSteals[i] <= False;
        end
    endmethod
    
    method Bool isDone;
        return done && engine.isDone();
    endmethod
    
    interface enq = map(toPut, enqQs);
    interface deq = map(toGet, deqQs);
    interface memReq = map(toGet, engine.memReq);
    interface memResp = map(toPut, engine.memResp);
endmodule

endpackage
