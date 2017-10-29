
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
    
    method Action init(BC_AEId fpgaId, BC_Addr lockLoc, BC_Addr headPtrLoc, BC_Addr tailPtrLoc, BC_Addr maxSize, BC_Addr bufferLoc, BC_Addr offsetLoc);
    method Bit#(64) getOffset();
    method Action stop();
    method Bool isDone();
endinterface


(* synthesize *)
module mkWorklistFIFO(Worklist);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) enqQs <- replicateM(mkFIFOF);
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) deqQs <- replicateM(mkFIFOF);
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) stealQs <- replicateM(mkSizedWireFIFOF(2));
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) interQs <- replicateM(mkSizedWireFIFOF(4));
    Vector#(`NUM_ENGINES, Reg#(Bool)) reqSteals <- replicateM(mkRegU);
    Vector#(`NUM_ENGINES, Reg#(UInt#(3))) priorities <- replicateM(mkRegU);
    Vector#(`NUM_ENGINES, Reg#(UInt#(3))) stealPri <- replicateM(mkRegU);
    Vector#(`NUM_ENGINES, Reg#(UInt#(8))) timer <- replicateM(mkRegU);
    
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) engineQs <- replicateM(mkSizedBufBRAMFIFOF(`WORKLIST_FIFO_SIZE));
    
    //Reg#(Vector#(`NUM_ENGINES, Bool)) noEnqs <- mkRegU;
    Reg#(Vector#(`NUM_ENGINES, Bool)) engineEmpties <- mkReg(replicate(False));
    Reg#(Vector#(`NUM_ENGINES, Bool)) stealQEmpties <- mkReg(replicate(False));
    Reg#(Vector#(`NUM_ENGINES, Bool)) enqQEmpties <- mkReg(replicate(False));
    Reg#(Vector#(`NUM_ENGINES, Bool)) deqQEmpties <- mkReg(replicate(False));
    Reg#(Vector#(`NUM_ENGINES, Bool)) interQEmpties <- mkReg(replicate(False));
    Reg#(Vector#(`NUM_ENGINES, Bool)) allPri7 <- mkReg(replicate(False));
    Reg#(Bool) done <- mkReg(True);
    Reg#(Bool) started <- mkReg(False);
    
    WLEngine engine <- mkWLEngine();
    
    Vector#(`NUM_ENGINES, Wire#(Bool)) enqValid <- replicateM(mkDWire(False));

    rule set_move(started);
        let cycle <- cur_cycle;
        function Bool isMove(Integer x) = (priorities[x] == 7);
	function Bool isTrue(Bool x) = x;
	function Bool isFalse(Bool x) = !x;

	Vector#(`NUM_ENGINES, Bool) allMove = genWith(isMove);
	//if (cycle % 1024 == 0) $display("%d: Priorities are %d %d %d %d", cur_cycle, priorities[0], priorities[1], priorities[2], priorities[3]);
	if (all(isTrue, allMove)) engine.cur_pri_ifc <= True;
	else if (all(isFalse, allMove)) engine.cur_pri_ifc <= False;
    endrule

    rule calcDone(started);
        //function Bool enqF(Integer x) = !enqValid[x];
	function Bool stealF(Integer x) = !stealQs[x].notEmpty;
        function Bool engineF(Integer x) = !engineQs[x].notEmpty;
        function Bool interF(Integer x) = !interQs[x].notEmpty;
	function Bool enqQsF(Integer x) = !enqQs[x].notEmpty;
	function Bool deqQsF(Integer x) = !deqQs[x].notEmpty;
        function Bool isTrue(Bool x) = x;
        //noEnqs <= genWith(enqF);
        engineEmpties <= genWith(engineF);
	stealQEmpties <= genWith(stealF);
	enqQEmpties <= genWith(enqQsF);
	deqQEmpties <= genWith(deqQsF);
        interQEmpties <= genWith(interF);
        
        //done <= all(isTrue, noEnqs) && all(isTrue, engineEmpties);
        done <= all(isTrue, stealQEmpties) && all(isTrue, engineEmpties) && all(isTrue, enqQEmpties) && all(isTrue, deqQEmpties) && all(isTrue, interQEmpties);
        let cycle <- cur_cycle;
        //if (any(isTrue, interQEmpties) && (cycle % 64 == 0)) $display("%0d: interQ: %0d%0d%0d%0d", cur_cycle, interQs[0].notEmpty, interQs[1].notEmpty, interQs[2].notEmpty, interQs[3].notEmpty);
        //if (any(isTrue, engineEmpties) && (cycle % 64 == 0)) $display("%0d: interQ: %0d%0d%0d%0d", cur_cycle, engineQs[0].notEmpty, engineQs[1].notEmpty, engineQs[2].notEmpty, engineQs[3].notEmpty);
	//if (`DEBUG) $display("mkWorklistFIFO[%0d]: WorklistFIFO done is %0d", fpgaId, done);
        //if (cycle % 1024 == 0) $display("FPGA[%0d]: engine:%0d%0d%0d%0d, inter:%0d%0d%0d%0d, steal:%0d%0d%0d%0d", fpgaId, engineQs[0].notEmpty, engineQs[1].notEmpty, engineQs[2].notEmpty, engineQs[3].notEmpty, interQs[0].notEmpty, interQs[1].notEmpty, interQs[2].notEmpty, interQs[3].notEmpty, stealQs[0].notEmpty, stealQs[1].notEmpty, stealQs[2].notEmpty, stealQs[3].notEmpty);



    endrule
    
    rule stealReqs(started);
        function Bool isTrue(Bool x) = x;
        function Bool stealsF(Integer x) = reqSteals[x];
        function Bool engineF(Integer x) = engineQs[x].notEmpty;
        Vector#(`NUM_ENGINES, Bool) steals = genWith(stealsF);
        Vector#(`NUM_ENGINES, Bool) engines = genWith(engineF);
        if(any(isTrue, steals)) begin
            //if(`DEBUG) $display("steals: %b, engineQs: %b", steals, engines);
        end
    endrule

    for(Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
        (* descending_urgency = "processEnq, processFill, setStealPri" *)
        //rule engineFull(started);
        //    if(!engineQs[i].notFull) begin
        //        if(`DEBUG) $display("WorklistFIFO engineQ[%0d] is full!", i);
        //    end
        //    let cycle <- cur_cycle;
            //if(cycle == 10000000) $display("WorklistFIFO[%0d][%0d] empties: enqQs: %b, deqQs: %b, stealQs: %b, reqSteals (reg): %b, engineQs: %b", fpgaId, i, !enqQs[i].notEmpty, !deqQs[i].notEmpty, !stealQs[i].notEmpty, reqSteals[i], !engineQs[i].notEmpty);
            //if(cycle == 10000000) $display("WorklistFIFO[%0d][%0d] fulls: enqQs: %b, deqQs: %b, stealQs: %b, reqSteals (reg): %b, engineQs: %b", fpgaId, i, !enqQs[i].notFull, !deqQs[i].notFull, !stealQs[i].notFull, reqSteals[i], !engineQs[i].notFull);
        //endrule
        
        //rule processPreFill(started && fillBuf[i].notFull);
	//    let pkt <- engine.streamOut[i].get;
	//    fillBuf[i].enq(pkt);
	//endrule

        rule processFill(started);
            Integer stealIdx = ?;
            if(i == `NUM_ENGINES-1)
                stealIdx = 0;
            else
                stealIdx = i + 1;
            let pkt <- engine.streamOut[i].get;
	    //fillBuf[i].deq();
	    //UInt#(3) pri = truncate(unpack(tpl_1(pkt)));
	    UInt#(3) pri;
	    pri = unpack(engine.priority_ifc[i]);
            engineQs[i].enq(pkt);
	    if ((!engineQs[i].notEmpty) || (pri < priorities[i])) begin
	        priorities[i] <= pri;
		stealPri[stealIdx] <= pri;
                //$display("%0d: WorklistFIFO filling engineQ[%0d][%0d], cur pri: %0d, new pri: %0d, notEmpty: %b ", cur_cycle, fpgaId, i, priorities[i], pri, engineQs[i].notEmpty, fshow(pkt));
	    end
	    else begin
                //$display("%0d: WorklistFIFO filling engineQ[%0d][%0d], cur pri: %0d, notEmpty: %b", cur_cycle, fpgaId, i, priorities[i], engineQs[i].notEmpty, fshow(pkt));
	    end
        endrule

	rule setStealPri(started && stealQs[i].notEmpty);
	    //WLEntry pkt = stealQs[i].first();
	    priorities[i] <= stealPri[i];
            //$display("%0d: StealQ[%0d][%0d] set priority, cur pri: %0d, new pri: %0d, notEmpty: %b ", cur_cycle, fpgaId, i, priorities[i], stealPri[i], engineQs[i].notEmpty);
	endrule
        
        rule processEnq(started);
            Integer stealIdx = ?;
            if(i == `NUM_ENGINES-1)
                stealIdx = 0;
            else
                stealIdx = i + 1;
            WLEntry pkt = enqQs[i].first;
            enqValid[i] <= True;
            UInt#(3) pri = truncate(unpack(tpl_1(pkt)));
	    timer[i] <= timer[i] + 1;
            if(engineQs[i].notFull && (timer[i] != 0)) begin
                if (pri <= priorities[i]) begin
                    $display("%0d: WorklistFIFO enqing engineQ[%0d][%0d], new pri is %0d, cur pri is %0d ", cur_cycle, fpgaId, i, pri, priorities[i]);
                    priorities[i] <= pri;
                    enqQs[i].deq();
		    stealPri[stealIdx] <= pri;
                    engineQs[i].enq(pkt);
                end
                else begin
                    enqQs[i].deq();
                    engine.streamIn[i].put(pkt);
                    $display("%0d: WorklistFIFO kick to streamIn[%0d][%0d], pkt pri: %0d, cur pri: %0d", cur_cycle, fpgaId, i, pri, priorities[i]);
		end
            end
            else begin
                enqQs[i].deq();
                engine.streamIn[i].put(pkt);
                $display("%0d: WorklistFIFO spill to streamIn[%0d][%0d]", cur_cycle, fpgaId, i);
            end
        endrule

	rule processEnq2(started);
            Integer stealIdx = ?;
            if(i == `NUM_ENGINES-1)
                stealIdx = 0;
            else
                stealIdx = i + 1;
            
	    WLEntry pkt = engineQs[i].first;
            if(reqSteals[stealIdx] && stealQs[stealIdx].notFull) begin
                stealQs[stealIdx].enq(pkt);
		//stealPri[stealIdx].enq(priorities[i]);
		engineQs[i].deq();
                //$display("WorklistFIFO enqing stealQ[%0d]", stealIdx, fshow(pkt));
            end
	    else if (interQs[i].notFull) begin
	        interQs[i].enq(pkt);
		engineQs[i].deq();
                //$display("WorklistFIFO enqing interQ[%0d]", i, fshow(pkt));
            end
	endrule
        
        rule processDeq(started && deqQs[i].notFull);
            if(stealQs[i].notEmpty) begin
                let pkt = stealQs[i].first();
                stealQs[i].deq();
                //if(fpgaId == 0 && i == 0) $display("Deq from stealQ[%0d]", i);
                deqQs[i].enq(pkt);
                reqSteals[i] <= False;
            end
            else if(interQs[i].notEmpty) begin
                let pkt = interQs[i].first();
                interQs[i].deq();
                //if(fpgaId == 0 && i == 0) $display("Deq from interQ[%0d]", i);
                deqQs[i].enq(pkt);
                reqSteals[i] <= False;
            end
            else begin
                reqSteals[i] <= True;
            end
        endrule
    end
    
    method Action init(BC_AEId fpgaid, BC_Addr lockloc, BC_Addr headptrloc, BC_Addr tailptrloc, BC_Addr maxsize, BC_Addr bufferloc, BC_Addr offsetloc);
        $display("%0d: mkWorklistFIFO[%0d]: INIT", cur_cycle, fpgaid);
        engine.init(fpgaid, lockloc, headptrloc, tailptrloc, maxsize, bufferloc, offsetloc);
        done <= False;
        started <= True;
        fpgaId <= fpgaid;
        
        for(Integer i = 0; i < `NUM_ENGINES; i=i+1) begin
            reqSteals[i] <= False;
            priorities[i] <= 7;
	    timer[i] <= 0;
        end
    endmethod

    method Action stop;
        started <= False;
        engine.stop();
    endmethod
    
    method Bit#(64) getOffset;
        return engine.getOffset();
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
