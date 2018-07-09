
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

`define COUNTER_MAX 640

interface Worklist;
    interface Vector#(`NUM_ENGINES, Put#(WLEntry)) enq;
    interface Vector#(`NUM_ENGINES, Get#(WLEntry)) deq;
    
    interface Vector#(16, Get#(MemReq)) memReq;
    interface Vector#(16, Put#(MemResp)) memResp;
    
    method Action init(BC_AEId fpgaId, BC_Addr lockLoc, BC_Addr headPtrLoc, BC_Addr tailPtrLoc, BC_Addr tailPtrLoc_w, BC_Addr commitHeadPtrLoc, BC_Addr commitTailPtrLoc, BC_Addr maxSize, BC_Addr bufferLoc, BC_Addr offsetLoc);
    method Bit#(64) getOffset();
    method Action stop();
    method Bool isDone();
endinterface


(* synthesize *)
module mkWorklistFIFO(Worklist);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) enqQs <- replicateM(mkSizedBufBRAMFIFOF(512));
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) deqQs <- replicateM(mkFIFOF);
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) stealQs <- replicateM(mkSizedWireFIFOF(2));
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) interQs <- replicateM(mkSizedWireFIFOF(4));
    Vector#(`NUM_ENGINES, Reg#(Bool)) reqSteals <- replicateM(mkRegU);
    Vector#(`NUM_ENGINES, Reg#(UInt#(3))) priorities <- replicateM(mkRegU);
    Vector#(`NUM_ENGINES, Reg#(UInt#(3))) pre_priorities <- replicateM(mkRegU);
    Vector#(`NUM_ENGINES, Reg#(UInt#(3))) stealPri <- replicateM(mkRegU);
    Vector#(`NUM_ENGINES, Reg#(UInt#(3))) backupPri <- replicateM(mkRegU);
    Vector#(`NUM_ENGINES, Reg#(UInt#(10))) counter <- replicateM(mkRegU);
    Vector#(`NUM_ENGINES, Reg#(Bool)) turn <- replicateM(mkReg(False));
    Vector#(`NUM_ENGINES, Reg#(Bool)) upgrade <- replicateM(mkReg(False));
    
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) engineQs <- replicateM(mkSizedBufBRAMFIFOF(`WORKLIST_FIFO_SIZE));
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) backupQs <- replicateM(mkSizedBufBRAMFIFOF(512));
    
    //Reg#(Vector#(`NUM_ENGINES, Bool)) noEnqs <- mkRegU;
    Reg#(Vector#(`NUM_ENGINES, Bool)) engineEmpties <- mkReg(replicate(False));
    Reg#(Vector#(`NUM_ENGINES, Bool)) stealQEmpties <- mkReg(replicate(False));
    Reg#(Vector#(`NUM_ENGINES, Bool)) enqQEmpties <- mkReg(replicate(False));
    Reg#(Vector#(`NUM_ENGINES, Bool)) deqQEmpties <- mkReg(replicate(False));
    Reg#(Vector#(`NUM_ENGINES, Bool)) backupEmpties <- mkReg(replicate(False));
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
        function Bool backupF(Integer x) = !backupQs[x].notEmpty;
	function Bool enqQsF(Integer x) = !enqQs[x].notEmpty;
	function Bool deqQsF(Integer x) = !deqQs[x].notEmpty;
        function Bool isTrue(Bool x) = x;
        //noEnqs <= genWith(enqF);
        engineEmpties <= genWith(engineF);
	stealQEmpties <= genWith(stealF);
	enqQEmpties <= genWith(enqQsF);
	deqQEmpties <= genWith(deqQsF);
        backupEmpties <= genWith(backupF);
        
        //done <= all(isTrue, noEnqs) && all(isTrue, engineEmpties);
        done <= all(isTrue, stealQEmpties) && all(isTrue, engineEmpties) && all(isTrue, enqQEmpties) && all(isTrue, deqQEmpties) && all(isTrue, backupEmpties);
        let cycle <- cur_cycle;
        //if (any(isTrue, interQEmpties) && (cycle % 64 == 0)) $display("%0d: interQ: %0d%0d%0d%0d", cur_cycle, interQs[0].notEmpty, interQs[1].notEmpty, interQs[2].notEmpty, interQs[3].notEmpty);
        //if (any(isTrue, engineEmpties) && (cycle % 64 == 0)) $display("%0d: interQ: %0d%0d%0d%0d", cur_cycle, engineQs[0].notEmpty, engineQs[1].notEmpty, engineQs[2].notEmpty, engineQs[3].notEmpty);
	//if (`DEBUG) $display("mkWorklistFIFO[%0d]: WorklistFIFO done is %0d", fpgaId, done);
        //if (cycle % 1024 == 0) $display("FPGA[%0d]: engine:%0d%0d%0d%0d, inter:%0d%0d%0d%0d, steal:%0d%0d%0d%0d", fpgaId, engineQs[0].notEmpty, engineQs[1].notEmpty, engineQs[2].notEmpty, engineQs[3].notEmpty, interQs[0].notEmpty, interQs[1].notEmpty, interQs[2].notEmpty, interQs[3].notEmpty, stealQs[0].notEmpty, stealQs[1].notEmpty, stealQs[2].notEmpty, stealQs[3].notEmpty);



    endrule
    
    //rule stealReqs(started);
    //    function Bool isTrue(Bool x) = x;
    //    function Bool stealsF(Integer x) = reqSteals[x];
    //    function Bool engineF(Integer x) = engineQs[x].notEmpty;
    //    Vector#(`NUM_ENGINES, Bool) steals = genWith(stealsF);
    //    Vector#(`NUM_ENGINES, Bool) engines = genWith(engineF);
    //    if(any(isTrue, steals)) begin
    //        //if(`DEBUG) $display("steals: %b, engineQs: %b", steals, engines);
    //    end
    //endrule

    for(Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
        //(* descending_urgency = "processEnq, processFill, setStealPri" *)
        (* descending_urgency = "processEnq, processFill" *)
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
	    //fillBuf[i].deq();
	    //UInt#(3) pri = truncate(unpack(tpl_1(pkt)));
            if(stealQs[i].notEmpty && engineQs[i].notFull) begin
                let pkt = stealQs[i].first();
                UInt#(3) pri = truncate(unpack(tpl_1(pkt)));
                stealQs[i].deq();
                //$display("Deq from stealQ[%0d]", i);
                engineQs[i].enq(pkt);
	        if (stealPri[i] <= priorities[i]) begin
		    priorities[i] <= stealPri[i];
		    counter[i] <= 0;
		end
		else if (counter[i] < `COUNTER_MAX) counter[i] <= counter[i] + 1;
            end
	    else if (engine.streamOut[i].notEmpty && engineQs[i].notFull) begin
                WLEntry pkt = engine.streamOut[i].first();
	        //let pkt <- engine.streamOut[i].get();
	        UInt#(3) pri;
	        pri = unpack(engine.priority_ifc[i]);
	        if (pri <= priorities[i]) counter[i] <= 0;
	        else if (counter[i] < `COUNTER_MAX) counter[i] <= counter[i] + 1;
	        if (pri <= backupPri[i] || (!backupQs[i].notEmpty)) begin
	            engine.streamOut[i].deq();
                    engineQs[i].enq(pkt);
	            //if ((pri < priorities[i]) || (!engineQs[i].notEmpty)) begin
		    if ((counter[i] == `COUNTER_MAX) || (pri < priorities[i])) begin
	                priorities[i] <= pri;
			turn[i] <= True;
		    end
                    //if (fpgaId == 0 && i == 0) $display("%0d: WorklistFIFO filling engineQ[%0d][%0d], cur pri: %0d, new pri: %0d, counter: %0d", cur_cycle, fpgaId, i, priorities[i], pri, counter[i]);
	            //end
	            //else begin
                        //$display("%0d: WorklistFIFO filling engineQ[%0d][%0d], cur pri: %0d, notEmpty: %b", cur_cycle, fpgaId, i, priorities[i], engineQs[i].notEmpty, fshow(pkt));
	            //end
	        end
		else if (backupQs[i].notEmpty) begin
		    WLEntry pkt2 = backupQs[i].first();
		    backupQs[i].deq();
		    engineQs[i].enq(pkt2);
                    //if (fpgaId == 0 && i == 0) $display("%0d: backupQ filling engineQ[%0d][%0d], cur pri: %0d, new pri: %0d, counter: %0d", cur_cycle, fpgaId, i, priorities[i], backupPri[i], counter[i]);
		    if (counter[i] == `COUNTER_MAX) begin
	                priorities[i] <= backupPri[i];
			turn[i] <= True;
		    end
		end
	    end
	    else if (backupQs[i].notEmpty && ((!engineQs[i].notEmpty) || turn[i]) && engineQs[i].notFull) begin
		WLEntry pkt2 = backupQs[i].first();
		backupQs[i].deq();
		engineQs[i].enq(pkt2);
                //if (fpgaId == 0 && i == 0) $display("%0d: backupQ filling engineQ[%0d][%0d], cur pri: %0d, new pri: %0d, counter: %0d", cur_cycle, fpgaId, i, priorities[i], backupPri[i], counter[i]);
	        if (counter[i] < `COUNTER_MAX) counter[i] <= counter[i] + 1;
		else if (counter[i] == `COUNTER_MAX) begin
	            priorities[i] <= backupPri[i];
		    turn[i] <= True;
		end
	    end 
	    else if (!(backupQs[i].notEmpty || engine.streamOut[i].notEmpty) || (!engineQs[i].notFull)) begin
	        if (counter[i] < `COUNTER_MAX) counter[i] <= counter[i] + 1;
	        turn[i] <= False;
	    	let cycle <- cur_cycle;
	    	//if (fpgaId == 0 && i == 0 && (cycle % 128 == 0)) $display("%0d: nothing to fill 0", cycle);
	    end
	    else begin
	        if (counter[i] < `COUNTER_MAX) counter[i] <= counter[i] + 1;
	    	let cycle <- cur_cycle;
	    	//if (fpgaId == 0 && i == 0 && (cycle % 128 == 0)) $display("%0d: nothing to fill 1", cycle);
	    end
        endrule

	//rule setStealPri(started && stealQs[i].notEmpty);
	//    //WLEntry pkt = stealQs[i].first();
	//    if (counter[i] == `COUNTER_MAX) begin
	//        priorities[i] <= stealPri[i];
	//    end
        //    //$display("%0d: StealQ[%0d][%0d] set priority, cur pri: %0d, new pri: %0d, notEmpty: %b ", cur_cycle, fpgaId, i, priorities[i], stealPri[i], engineQs[i].notEmpty);
	//endrule
        
        rule processEnq(started && (!turn[i]) && enqQs[i].notEmpty);
            Integer stealIdx = ?;
            if(i == `NUM_ENGINES-1)
                stealIdx = 0;
            else
                stealIdx = i + 1;
            enqValid[i] <= True;
            WLEntry pkt = enqQs[i].first;
            UInt#(3) pri = truncate(unpack(tpl_1(pkt)));
	    if (pri <= priorities[i]) counter[i] <= 0;
	    else if (counter[i] < `COUNTER_MAX) counter[i] <= counter[i] + 1;
            if(engineQs[i].notFull) begin
                if (pri <= priorities[i]) begin
                //if (pri <= priorities[i] || (!engineQs[i].notEmpty)) begin
                    //$display("%0d: WorklistFIFO enqing engineQ[%0d][%0d], node id is %d, new pri is %0d, cur pri is %0d ", cur_cycle, fpgaId, i, tpl_2(pkt), pri, priorities[i]);
            	    if(reqSteals[stealIdx] && stealQs[stealIdx].notFull) begin
                        stealQs[stealIdx].enq(pkt);
                    	enqQs[i].deq();
		    	stealPri[stealIdx] <= pri;
			//backupPri[stealIdx] <= pri+1;
		    end
		    else begin
	            	backupPri[i] <= (pri == 7) ? 7 : pri+1;
                    	enqQs[i].deq();
                    	priorities[i] <= pri;
                    	engineQs[i].enq(pkt);
		    end
                end
                else begin
		    backupPri[i] <= (priorities[i] == 7) ? 7 : priorities[i]+1;
		    if (pri == backupPri[i] && backupQs[i].notFull) begin
		        backupQs[i].enq(pkt);
			enqQs[i].deq();
                        //$display("%0d: WorklistFIFO get in backupQ[%0d][%0d], node id is %0d, pkt pri: %0d, cur pri: %0d", cur_cycle, fpgaId, i, tpl_2(pkt), pri, priorities[i]);
		    end
		    else begin
                        enqQs[i].deq();
                        engine.streamIn[i].put(pkt);
                        //$display("%0d: WorklistFIFO kick to streamIn[%0d][%0d], node id is %0d, pkt pri: %0d, cur pri: %0d", cur_cycle, fpgaId, i, tpl_2(pkt), pri, priorities[i]);
		    end
		end
            end
            else begin
                enqQs[i].deq();
                engine.streamIn[i].put(pkt);
                //$display("%0d: WorklistFIFO spill to streamIn[%0d][%0d], node id is %0d", cur_cycle, fpgaId, i, tpl_2(pkt));
            end
        endrule

        rule processDeq(started);
            if(engineQs[i].notEmpty && deqQs[i].notFull) begin
                let pkt = engineQs[i].first();
                engineQs[i].deq();
                //$display("Deq from engineQs[%0d]", i);
                deqQs[i].enq(pkt);
                reqSteals[i] <= False;
            end
            else if (deqQs[i].notFull) begin
                reqSteals[i] <= True;
            end
        endrule
    end
    
    method Action init(BC_AEId fpgaid, BC_Addr lockloc, BC_Addr headptrloc, BC_Addr tailptrloc, BC_Addr tailptrloc_w, BC_Addr commitheadptrloc, BC_Addr committailptrloc, BC_Addr maxsize, BC_Addr bufferloc, BC_Addr offsetloc);
        $display("%0d: mkWorklistFIFO[%0d]: INIT", cur_cycle, fpgaid);
        //engine.init(fpgaid, lockloc, headptrloc, tailptrloc, tailptrloc_w, commitheadptrloc, committailptrloc, maxsize, bufferloc, offsetloc);
        engine.init(fpgaid, lockloc, headptrloc, tailptrloc, maxsize, bufferloc, offsetloc);
        done <= False;
        started <= True;
        fpgaId <= fpgaid;
        
        for(Integer i = 0; i < `NUM_ENGINES; i=i+1) begin
            reqSteals[i] <= False;
            priorities[i] <= 0;
	    backupPri[i] <= 0;
	    counter[i] <= 0;
	    turn[i] <= False;
	    pre_priorities[i] <= 0;
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
