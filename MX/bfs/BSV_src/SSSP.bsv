
package SSSP;

import Vector           :: *;
import FIFOF            :: *;
import SpecialFIFOs     :: *;
import GetPut           :: *;
import ClientServer     :: *;
import Connectable      :: *;
import StmtFSM          :: *;
import FShow            :: *;
import BRAMFIFO         :: *;

// ----------------
// BC library imports

import BC_Utils           :: *;
import BC_HW_IFC          :: *;
import BC_Transactors     :: *;

import Clocks::*;

import SSSPEngine::*;
import WorklistFIFO::*;
import GraphEngine::*;
import GaloisTypes::*;
`include "GaloisDefs.bsv"

`define WATCHDOG_TIMEOUT 1000000000
//`define WATCHDOG_TIMEOUT 50000000
//`define WATCHDOG_TIMEOUT 100000

interface BC_HW2_IFC;
   method Action start (BC_AEId fpga_id, BC_Data param_block_addr);
   method Action waitTillDone;

   interface Vector #(16, BC_MC_Client) mc_ifcs;
endinterface: BC_HW2_IFC


Integer param_nodePtr = 0;
Integer param_edgePtr = 1;
Integer param_jobsPtr = 2;
Integer param_metaPtr = 3;
Integer param_output = 4;
Integer param_donePtr = 5;
Integer param_sentinel = 6;

Integer param_lock = 0;
Integer param_headPtr = 2;
Integer param_tailPtr = 3;
Integer param_wlSize = 4;
Integer param_numFPGA = 5;
Integer param_tailPtr_w = 6;
Integer param_commitHead = 7;
Integer param_commitTail = 8;

(* synthesize *)
module mkSSSP(BC_HW2_IFC);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Reg#(BC_Addr) paramPtr <- mkRegU;
    Reg#(BC_Addr) paramNodePtr  <- mkRegU;
    Reg#(BC_Addr) paramEdgePtr  <- mkRegU;
    Reg#(BC_Addr) paramJobsPtr  <- mkRegU;
    Reg#(BC_Addr) paramMetaPtr  <- mkRegU;
    Reg#(BC_Addr) paramOutputPtr<- mkRegU;
    Reg#(BC_Addr) paramDonePtr  <- mkRegU;
    Reg#(BC_Addr) paramSentinel <- mkRegU;
    Reg#(UInt#(2)) rg_numFPGA <- mkRegU;
    
    Reg#(Bit#(`NUM_ENGINES)) engineDoneIdx <- mkRegU; // need +1 for terminating condition
    Reg#(Bool) done <- mkRegU;
    Reg#(Bit#(5)) numDones <- mkRegU;
    Reg#(Bool) allDone <- mkReg(True);
    Reg#(Bit#(4)) numAllDones <- mkRegU;
    Reg#(Bool) doneResetting_pre <- mkReg(False);
    Reg#(Bool) doneResetting <- mkReg(False);
    Reg#(Bool) incremented <- mkRegU;
    Reg#(Bit#(64)) watchdog <- mkRegU;
    Reg#(Bit#(64)) cycle_counter <- mkRegU;
    Reg#(Bit#(64)) memCounter <- mkRegU;
    
    Clock clk <- exposeCurrentClock;
    Reset rst <- exposeCurrentReset;
    
    Vector#(`NUM_ENGINES, MakeResetIfc) engineRsts <- replicateM(mkReset(1, False, clk));
    MakeResetIfc graphRst <- mkReset(1, False, clk);
    MakeResetIfc worklistRst <- mkReset(1, False, clk);
    
    Vector#(16, Reg#(Bit#(64))) memCounterGraph <- replicateM(mkRegU);
    Vector#(16, Reg#(Bit#(64))) memCounterWorklist <- replicateM(mkRegU);
    Vector#(16, Reg#(Bit#(64))) memCounterSSSP <- replicateM(mkRegU);

    Vector#(16, FIFOF#(BC_MC_REQ)) memReqQ  <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(BC_MC_RSP)) memRespQ <- replicateM(mkFIFOF);
    Vector #(16, FIFOF #(BC_MC_flush_req)) f_flush_reqs <- replicateM (mkFIFOF);
    Vector #(16, FIFOF #(BC_MC_flush_rsp)) f_flush_rsps <- replicateM (mkFIFOF);

    Vector#(16, FIFOF#(MemReq)) ssspOutQs <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemResp)) ssspInQs  <- replicateM(mkFIFOF);
    
    Vector#(`NUM_ENGINES, Engine) engines;
    for(Integer i = 0; i < `NUM_ENGINES; i=i+1) begin
        engines[i] <- mkSSSPEngine(reset_by engineRsts[i].new_rst);
    end
    //Vector#(`NUM_ENGINES, Reg#(Bit#(64))) engineResults <- replicateM(mkRegU);
    Reg#(Bit#(64)) engineResult <- mkRegU;
    Reg#(Bit#(64)) engineNodes <- mkRegU;
    Reg#(Bit#(64)) engineRetry <- mkRegU;
    Reg#(Bit#(64)) edgePipeStall <- mkRegU;
    Reg#(Bit#(64)) worklistStall <- mkRegU;
    Reg#(UInt#(5)) rg_i <- mkRegU;
    Reg#(Bool) set_done <- mkReg(False);
    
    Worklist worklist <- mkWorklistFIFO(reset_by worklistRst.new_rst);
    Vector#(16, FIFOF#(MemReq)) worklistOutQs <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemResp)) worklistInQs  <- replicateM(mkFIFOF);
    
    GraphEngine graph <- mkGraphEngine(reset_by graphRst.new_rst);
    Vector#(16, FIFOF#(MemReq)) graphOutQs <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemResp)) graphInQs  <- replicateM(mkFIFOF);
    
    rule watchdogInc;
        watchdog <= watchdog + 1;
    endrule

    rule cycle_count(!allDone);
        cycle_counter <= cycle_counter + 1;
    endrule
    
    rule setDoneResetting;
        doneResetting <= doneResetting_pre;
    endrule
    
    function BC_MC_REQ memReqToBC(MemReq req);
        if(req matches tagged MemRead64 .mem) begin
            return BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: extend(pack(mem.gaddr)), len: BC_8B, vadr: mem.addr, data: ?};
        end
        else if(req matches tagged MemRead32 .mem) begin
            return BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: extend(pack(mem.gaddr)), len: BC_4B, vadr: mem.addr, data: ?};
        end
        else if(req matches tagged MemWrite64 .mem) begin
            return BC_MC_REQ{cmd_sub: REQ_WR, rtnctl: extend(pack(mem.gaddr)), len: BC_8B, vadr: mem.addr, data: mem.data};
        end
        else if(req matches tagged MemWrite32 .mem) begin
            return BC_MC_REQ{cmd_sub: REQ_WR, rtnctl: extend(pack(mem.gaddr)), len: BC_4B, vadr: mem.addr, data: extend(mem.data)};
        end
        else if(req matches tagged MemCAS32 .mem) begin
            return BC_MC_REQ{cmd_sub: REQ_ATOM_CAS, rtnctl: extend(pack(mem.gaddr)), len: BC_4B, vadr: mem.addr, data: {mem.cmpVal, mem.swapVal}};
        end
        else begin
            //$display("INVALID!");
            BC_MC_REQ r = ?;
            return r;
        end
    endfunction
    
    function MemResp bcToMemResp(BC_MC_RSP rsp);
        return MemResp{gaddr: unpack(truncate(rsp.rtnctl)), data: rsp.data};
    endfunction
    
    for(Integer i = 0; i < `NUM_ENGINES; i=i+1) begin
        mkConnection(engines[i].workOut, worklist.enq[i]);
        mkConnection(worklist.deq[i], engines[i].workIn);
        
        for(Integer j = 0; j < 2; j=j+1)
            mkConnection(engines[i].graphNodeReqs[j], graph.req[i].nodeReq[j]);
        for(Integer j = 0; j < 1; j=j+1)
            mkConnection(engines[i].graphEdgeReqs[j], graph.req[i].edgeReq[j]);
        for(Integer j = 0; j < 1; j=j+1)
            mkConnection(engines[i].graphCASReqs[j], graph.req[i].casReq[j]);
        
        for(Integer j = 0; j < 2; j=j+1)
            mkConnection(graph.resp[i].nodeResp[j], engines[i].graphNodeResps[j]);
        for(Integer j = 0; j < 1; j=j+1)
            mkConnection(graph.resp[i].edgeResp[j], engines[i].graphEdgeResps[j]);
        for(Integer j = 0; j < 1; j=j+1)
            mkConnection(graph.resp[i].casResp[j], engines[i].graphCASResps[j]);
        
    end
    
    /*
    rule print;
        function Bool goutEmptyF(Integer x) = !graphOutQs[x].notEmpty;
        function Bool goutFullF(Integer x) = !graphOutQs[x].notFull;
        function Bool ginEmptyF(Integer x) = !graphInQs[x].notEmpty;
        function Bool ginFullF(Integer x) = !graphInQs[x].notFull;
        Vector#(16, Bool) reqEmpty = genWith(goutEmptyF);
        Vector#(16, Bool) respEmpty = genWith(ginEmptyF);
        Vector#(16, Bool) reqFull = genWith(goutFullF);
        Vector#(16, Bool) respFull = genWith(ginFullF);
        
        let cycle <- cur_cycle;
        if(cycle > 100000) $display("%0d: SSSP[%0d] graphOutQs empty:%b full:%b graphInQs empty:%b memRespQ full:%b", cur_cycle, fpgaId, 
           reqEmpty, reqFull, respEmpty, respFull);
    endrule
    */
    
    for(Integer i = 0; i < 16; i = i + 1) begin        
        mkConnection(worklist.memReq[i], toPut(worklistOutQs[i]));
        mkConnection(toGet(worklistInQs[i]), worklist.memResp[i]);
        
        
        mkConnection(graph.memReq[i], toPut(graphOutQs[i]));
        mkConnection(toGet(graphInQs[i]), graph.memResp[i]);
        
        rule toMem(doneResetting && (worklistOutQs[i].notEmpty || graphOutQs[i].notEmpty || ssspOutQs[i].notEmpty));
            if(worklistOutQs[i].notEmpty) begin
                MemReq req = worklistOutQs[i].first();
                worklistOutQs[i].deq();
                memReqQ[i].enq(memReqToBC(req));
                memCounterWorklist[i] <= memCounterWorklist[i] + 1;
                if(`DEBUG) $display("toMem WorkList routing to mem %0d ", i, fshow(memReqToBC(req)));
            end
            else if(graphOutQs[i].notEmpty) begin
                MemReq req = graphOutQs[i].first();
                graphOutQs[i].deq();
                memReqQ[i].enq(memReqToBC(req));
                memCounterGraph[i] <= memCounterGraph[i] + 1;
                if(`DEBUG) $display("toMem Graph routing to mem %0d ", i, fshow(memReqToBC(req)));
            end
            else if(ssspOutQs[i].notEmpty) begin
                MemReq req = ssspOutQs[i].first();
                ssspOutQs[i].deq();
                memReqQ[i].enq(memReqToBC(req));
                memCounterSSSP[i] <= memCounterSSSP[i] + 1;
                if(`DEBUG) $display("toMem SSSP routing to mem %0d ", i, fshow(memReqToBC(req)));
            end 
        endrule
        
        // rtnctl: 0 means it's for mkSSSP
        rule fromMem(doneResetting);
            BC_MC_RSP resp = memRespQ[i].first();
            memRespQ[i].deq();
            
            GaloisAddress gaddr = unpack(truncate(resp.rtnctl));
            //$display("Received packed gaddr %d", gaddr);
            if(gaddr.mod == MK_WORKLIST) begin
                if (`DEBUG) $display("fromMem packet routing to Worklist %0d", i);
                worklistInQs[i].enq(bcToMemResp(resp));
            end
            else if(gaddr.mod == MK_GRAPH) begin
                if (`DEBUG) $display("fromMem packet routing to GraphEngine %0d", i);
                graphInQs[i].enq(bcToMemResp(resp));
            end
            else if(gaddr.mod == MK_SSSP) begin
                if(`DEBUG) $display("fromMem packet routing to SSSP %0d", i);
                ssspInQs[i].enq(bcToMemResp(resp));
            end
            else begin
                //$display("ERROR: fromMem packet dest unknown: ", fshow(resp));
            end
        endrule
    end
    
    function Action send_rd_req(BC_Addr base, Integer param_id);
        action
	        let addr = base + fromInteger(param_id * 64);
	        //let req = BC_MC_REQ {cmd_sub: REQ_RD, rtnctl: pack(GaloisAddress{mod: MK_SSSP, addr: 0}), len: BC_8B, vadr: addr, data: ?};
            MemReq req = MemRead64{addr: addr, gaddr: GaloisAddress{mod: MK_SSSP, addr: 0}};
	        ssspOutQs[param_id].enq(req);
            //$display("%0d: mkSSSP[%0d]: send_rd_eq on channel %0d, addr: %0x", cur_cycle, fpgaId, param_id, addr);
        endaction
    endfunction
    
    function Action send_wr_req(BC_Addr base, Integer param_id, BC_Data x);
        action
	        let addr = base + fromInteger(param_id * 64);
	        //let req = BC_MC_REQ {cmd_sub: REQ_WR, rtnctl: pack(GaloisAddress{mod: MK_SSSP, addr: 0}), len: BC_8B, vadr: addr, data: x};
            MemReq req = MemWrite64{addr: addr, gaddr: GaloisAddress{mod: MK_SSSP, addr: 0}, data: x};
	        ssspOutQs[param_id].enq(req);
        endaction
    endfunction
    
    function ActionValue #(Bit#(64)) recv_rd_rsp(Integer param_id);
        actionvalue
	        let rsp <- toGet(ssspInQs[param_id]).get;            
	        return rsp.data;
        endactionvalue
    endfunction
    
    function Action recv_wr_rsp(Integer param_id);
        action
	        let rsp <- toGet(ssspInQs[param_id]).get;
        endaction
    endfunction
    
    // Initialization FSM. Sets up environment variables and starts the engines
    let fsm <- mkFSM(
       seq
           // Handle all the resets!
           action
               for(Integer i = 0; i < `NUM_ENGINES; i=i+1) action
                   engineRsts[i].assertReset();
               endaction
               graphRst.assertReset();
               worklistRst.assertReset();
           endaction
           action
               noAction;
           endaction
           
           action
               doneResetting_pre <= True;
           endaction
       
	       // Send read requests for the parameters for this FPGA (in parallel)
	       action
               $display("%0d: mkSSSP[%0d]: FSM sending...", cur_cycle, fpgaId);
	           send_rd_req (paramPtr, param_nodePtr);
	           send_rd_req (paramPtr, param_edgePtr);
	           send_rd_req (paramPtr, param_jobsPtr);
	           send_rd_req (paramPtr, param_metaPtr);
	           send_rd_req (paramPtr, param_output);
	           send_rd_req (paramPtr, param_donePtr);
	           send_rd_req (paramPtr, param_sentinel);
	       endaction

	       // Receive the parameters for this FPGA (read responses, in parallel)
	       action
	           let nodePtr   <- recv_rd_rsp(param_nodePtr);
               paramNodePtr <= truncate(nodePtr);
           endaction
           action
	           let edgePtr   <- recv_rd_rsp(param_edgePtr);
               paramEdgePtr <= truncate(edgePtr);
           endaction
           action
	           let jobsPtr   <- recv_rd_rsp(param_jobsPtr);
               paramJobsPtr <= truncate(jobsPtr);
           endaction
           action
	           let metaPtr <- recv_rd_rsp(param_metaPtr);
               paramMetaPtr <= truncate(metaPtr);
           endaction
           action
	           let outputPtr <- recv_rd_rsp(param_output);
               paramOutputPtr <= truncate(outputPtr);
           endaction
           action
	           let donePtr   <- recv_rd_rsp(param_donePtr);
               paramDonePtr <= truncate(donePtr);
           endaction
           action
	           let sentinel  <- recv_rd_rsp(param_sentinel);
               paramSentinel <= truncate(sentinel);
           endaction
           action
               $display ("%0d: mkSSSP [%0d]: params are %0h 0x%0h 0x%0h 0x%0h %0h %0h %0h", cur_cycle, fpgaId, paramNodePtr, paramEdgePtr, paramJobsPtr, paramMetaPtr, paramOutputPtr, paramDonePtr, paramSentinel);
           endaction
           
           // Read metadata
           action
               GaloisAddress rtn = GaloisAddress{mod: MK_SSSP, addr: 0};
               ssspOutQs[0].enq(MemRead64{addr: paramMetaPtr+fromInteger(param_lock*8), gaddr: rtn});
               ssspOutQs[2].enq(MemRead64{addr: paramMetaPtr+fromInteger(param_headPtr*8), gaddr: rtn});
               ssspOutQs[3].enq(MemRead64{addr: paramMetaPtr+fromInteger(param_tailPtr*8), gaddr: rtn});
               ssspOutQs[4].enq(MemRead64{addr: paramMetaPtr+fromInteger(param_wlSize*8), gaddr: rtn});
               ssspOutQs[5].enq(MemRead64{addr: paramMetaPtr+fromInteger(param_numFPGA*8), gaddr: rtn});
       endaction
           
           action
               let lock <- recv_rd_rsp(param_lock);
               let headPtr <- recv_rd_rsp(param_headPtr);
               let tailPtr <- recv_rd_rsp(param_tailPtr);
               let wlSize <- recv_rd_rsp(param_wlSize);
               $display("%0d: mkSSSP[%0d]: lock: %0d, headPtr: %0d, tailPtr: %0d, wlSize: %0d", cur_cycle, fpgaId, lock, headPtr, tailPtr, wlSize);
               BC_Addr lockLoc = paramMetaPtr + fromInteger(param_lock) * 8;
               BC_Addr headPtrLoc = paramMetaPtr + fromInteger(param_headPtr) * 8;
               BC_Addr tailPtrLoc = paramMetaPtr + fromInteger(param_tailPtr) * 8;
               BC_Addr tailPtrLoc_w = paramMetaPtr + fromInteger(param_tailPtr_w) * 8;
               BC_Addr commitHeadPtrLoc = paramMetaPtr + fromInteger(param_commitHead) * 8;
               BC_Addr commitTailPtrLoc = paramMetaPtr + fromInteger(param_commitTail) * 8;
               
               worklist.init(fpgaId, lockLoc, headPtrLoc, tailPtrLoc, tailPtrLoc_w, commitHeadPtrLoc, commitTailPtrLoc, truncate(wlSize), paramJobsPtr);
           endaction
           
           action
               graph.init(fpgaId, paramNodePtr, paramEdgePtr);
           endaction
           /*
           action
               for(Integer i = 0; i < `NUM_ENGINES; i=i+1) action
                   engineGraphChannels[i].init(fromInteger(i));
               endaction
           endaction
            */
           action
               // Start the N engines
	           for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) action
	               engines[i].init(fpgaId, fromInteger(i));
	           endaction
           endaction
       
           // Wait for completion
           action
               allDone <= False;
               numAllDones <= 1;
               done <= False;
               numDones <= 0;
	       let numFPGA <- recv_rd_rsp(param_numFPGA);
	       rg_numFPGA <= truncate(unpack(numFPGA));
           endaction
           
           while(numAllDones < 15 && watchdog < `WATCHDOG_TIMEOUT) seq
               numDones <= 0;
               set_done <= False;
               //$display("mkSSSP[%0d]: Checking local dones..., numAllDones = %d", fpgaId, numAllDones);
               while(numDones < 31 && watchdog < `WATCHDOG_TIMEOUT) seq
                   //$display("%0d: SSSP[%0d]: Checking allDones %0d...", cur_cycle, fpgaId, numAllDones);
                   done <= True;
                   action
                     if(!worklist.isDone) begin
                         done <= False;
                         //$display("mkSSSP[%0d]: WL not done!", fpgaId);
                     end
                   endaction
	           for (engineDoneIdx <= 0; engineDoneIdx < fromInteger(`NUM_ENGINES); engineDoneIdx <= engineDoneIdx + 1) action
                     if(!engines[engineDoneIdx].isDone) begin
                         done <= False;
                         //$display("%0d: SSSP[%0d]: Engine %0d not done!", cur_cycle, fpgaId, engineDoneIdx);
                     end
	           endaction
                   action
                       if(done) begin
                           numDones <= numDones + 1;
		           set_done <= False;
	               end
                       else begin
                           numDones <= 0;
                           numAllDones <= 1;
		           set_done <= True;
                           //if (`DEBUG) $display("mkSSSP[%0d]: RESETTING ALL DONE", fpgaId);
                       end
                   endaction

                   if (set_done) seq
                       action
                           BC_Addr addr = paramDonePtr + (extend(fpgaId) << 3);
                           ssspOutQs[0].enq(MemWrite64{addr: addr, gaddr: GaloisAddress{mod: MK_SSSP, addr: 0}, data: 1});
                       endaction
		   
		       action
		           ssspInQs[0].deq();
		       endaction
		   endseq

               endseq
            
               // Set Done
               
               if(watchdog < `WATCHDOG_TIMEOUT) seq
                   action
                       //$display("mkSSSP[%0d]: Local is all done!, numAllDones = %d", fpgaId, numAllDones);
                       BC_Addr addr = paramDonePtr + (extend(fpgaId) << 3);
                       ssspOutQs[0].enq(MemWrite64{addr: addr, gaddr: GaloisAddress{mod: MK_SSSP, addr: 0}, data: extend(numAllDones)});
                   endaction
                   
                   action
                       ssspInQs[0].deq();
                   endaction
                   
                   // Check Dones from all FPGAs
                   action
                       BC_Addr addr = paramDonePtr + fromInteger(0*8);
                       ssspOutQs[0].enq(MemRead64{addr: addr, gaddr: GaloisAddress{mod: MK_SSSP, addr: 0}});
                       addr = paramDonePtr + fromInteger(1*8);
                       ssspOutQs[1].enq(MemRead64{addr: addr, gaddr: GaloisAddress{mod: MK_SSSP, addr: 0}});
                       addr = paramDonePtr + fromInteger(2*8);
                       ssspOutQs[2].enq(MemRead64{addr: addr, gaddr: GaloisAddress{mod: MK_SSSP, addr: 0}});
                       addr = paramDonePtr + fromInteger(3*8);
                       ssspOutQs[3].enq(MemRead64{addr: addr, gaddr: GaloisAddress{mod: MK_SSSP, addr: 0}});
                   endaction
                   
                   action
	               let data0 <- recv_rd_rsp(0);
	               let data1 <- recv_rd_rsp(1);
	               let data2 <- recv_rd_rsp(2);
	               let data3 <- recv_rd_rsp(3);
                       Bit#(4) d0 = truncate(data0);
                       Bit#(4) d1 = truncate(data1);
                       Bit#(4) d2 = truncate(data2);
                       Bit#(4) d3 = truncate(data3);
		       //$display("rg_numFPGA is %d, d0: %d, d1: %d, d2: %d, d3: %d", rg_numFPGA, d0, d1, d2, d3);
       
                       if((d0 >= numAllDones) && 
		          ((d1 >= numAllDones) || (rg_numFPGA < 1)) && 
			  ((d2 >= numAllDones) || (rg_numFPGA < 2)) && 
			  ((d3 >= numAllDones) || (rg_numFPGA < 3))) begin
                           numAllDones <= numAllDones + 1;
                           incremented <= True;
                           $display("mkSSSP[%0d]: All done! numAllDones = %0d", fpgaId, numAllDones);
                       end
                       else begin
                           incremented <= False;
                           //$display("mkSSSP[%0d]: All dones: %0d %0d %0d %0d, numAllDone: %0d", fpgaId, data0, data1, data2, data3, numAllDones);
                       end
                   endaction
               
                   /*
                   if(incremented) seq
                       action
                           BC_Addr addr = paramDonePtr + (extend(fpgaId) << 3);
                           ssspOutQs[0].enq(BC_MC_REQ{cmd_sub: REQ_WR, rtnctl: pack(GaloisAddress{mod: MK_SSSP, addr: 0}), len: BC_8B, vadr: addr, data: fromInteger(0)});
                           $display("mkSSSP[%0d]: Not all done, writing 0!", fpgaId);
                       endaction
                       
                       action
                           ssspInQs[0].deq();
                       endaction
                   endseq
                   */
               endseq
           endseq

           action
               worklist.stop();
	       for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) action
	           engines[i].stop();
	       endaction
               allDone <= True;
           endaction
                              
           $display("%0d: SSSP[%0d]: All Done!", cur_cycle, fpgaId);
           for(rg_i <= 0; rg_i < `NUM_ENGINES; rg_i <= rg_i + 1) action
               let result <- engines[rg_i].result;
               let retry <- engines[rg_i].numCASRetry;
	       let epStall <- engines[rg_i].numEPStall;
	       let wlStall <- engines[rg_i].numWLStall;
	       let numNodes <- engines[rg_i].numNodes;
	       edgePipeStall <= edgePipeStall + epStall;
	       worklistStall <= worklistStall + wlStall;
	       engineResult <= engineResult + result;
	       engineNodes <= engineNodes + numNodes;
               engineRetry <= engineRetry + retry;
               $display("Engine[%0d][%0d] edges fetched = %0d", fpgaId, rg_i, result);
               $display("Engine[%0d][%0d] num CAS Retried = %0d", fpgaId, rg_i, retry);
           endaction
           
           for (rg_i <= 0; rg_i < 16; rg_i <= rg_i + 1) action
	       memCounter <= memCounter + memCounterWorklist[rg_i];
	   endaction
           for (rg_i <= 0; rg_i < 16; rg_i <= rg_i + 1) action
	       memCounter <= memCounter + memCounterGraph[rg_i];
	   endaction
           for (rg_i <= 0; rg_i < 16; rg_i <= rg_i + 1) action
	       memCounter <= memCounter + memCounterSSSP[rg_i];
	   endaction

           action
               BC_Addr addr0 = paramOutputPtr + (extend(fpgaId) << 6);
               BC_Addr addr1 = paramOutputPtr + (extend(fpgaId) << 6) + 48'd8;
               BC_Addr addr2 = paramOutputPtr + (extend(fpgaId) << 6) + 48'd16;
               BC_Addr addr3 = paramOutputPtr + (extend(fpgaId) << 6) + 48'd24;
               BC_Addr addr4 = paramOutputPtr + (extend(fpgaId) << 6) + 48'd32;
               BC_Addr addr5 = paramOutputPtr + (extend(fpgaId) << 6) + 48'd40;
               ssspOutQs[10].enq(MemWrite64{addr: addr0, gaddr: GaloisAddress{mod: MK_SSSP, addr: 0}, data: engineNodes});
               ssspOutQs[11].enq(MemWrite64{addr: addr1, gaddr: GaloisAddress{mod: MK_SSSP, addr: 0}, data: engineResult});
               ssspOutQs[12].enq(MemWrite64{addr: addr2, gaddr: GaloisAddress{mod: MK_SSSP, addr: 0}, data: engineRetry});
               ssspOutQs[13].enq(MemWrite64{addr: addr3, gaddr: GaloisAddress{mod: MK_SSSP, addr: 0}, data: edgePipeStall});
               ssspOutQs[14].enq(MemWrite64{addr: addr4, gaddr: GaloisAddress{mod: MK_SSSP, addr: 0}, data: worklistStall});
               ssspOutQs[15].enq(MemWrite64{addr: addr5, gaddr: GaloisAddress{mod: MK_SSSP, addr: 0}, data: memCounter});
           endaction
           
           action
               ssspInQs[10].deq();
               ssspInQs[11].deq();
	       ssspInQs[12].deq();
	       ssspInQs[13].deq();
               ssspInQs[14].deq();
               ssspInQs[15].deq();
           endaction
	       
           //action
           //for (Integer i = 0; i < 16; i = i + 1) action
           //    BC_Addr addr = paramOutputPtr + (extend(fpgaId) << 9) + 48'd16 + (fromInteger(i) << 3);
           //    ssspOutQs[i].enq(MemWrite64{addr: addr, gaddr: GaloisAddress{mod: MK_SSSP, addr: 0}, data: memCounterWorklist[i]});
           //endaction
           //endaction
           //
           //action
           //for (Integer i = 0; i < 16; i = i + 1) action
           //    ssspInQs[i].deq();
           //endaction
           //endaction

           //action
           //for (Integer i = 0; i < 16; i = i + 1) action
           //    BC_Addr addr = paramOutputPtr + (extend(fpgaId) << 9) + 48'd144 + (fromInteger(i) << 3);
           //    ssspOutQs[i].enq(MemWrite64{addr: addr, gaddr: GaloisAddress{mod: MK_SSSP, addr: 0}, data: memCounterGraph[i]});
           //endaction
           //endaction

           //action
           //for (Integer i = 0; i < 16; i = i + 1) action
           //    ssspInQs[i].deq();
           //endaction
           //endaction

           //action
           //for (Integer i = 0; i < 16; i = i + 1) action
           //    BC_Addr addr = paramOutputPtr + (extend(fpgaId) << 9) + 48'd272 + (fromInteger(i) << 3);
           //    ssspOutQs[i].enq(MemWrite64{addr: addr, gaddr: GaloisAddress{mod: MK_SSSP, addr: 0}, data: memCounterSSSP[i]});
           //endaction
           //endaction

           //action
           //for (Integer i = 0; i < 16; i = i + 1) action
           //    ssspInQs[i].deq();
           //endaction
           //endaction
	       // Write final (per-FPGA) sum back to param block, and drain the response
	       //send_wr_req (paramPtr, param_output, 64'hCAFEBABE_BEEFBEEF);
	       //send_wr_req (paramPtr, param_output, engineResult);
               send_wr_req (paramPtr, param_sentinel, cycle_counter);
	       //recv_wr_rsp (param_output);
               recv_wr_rsp (param_sentinel);
           doneResetting_pre <= False;
       endseq
       );

    
/*    function BC_MC_Client fn_mkMC_Client(Integer mc);
        return interface BC_MC_Client;
            interface req_rsp = interface Client;
                interface request = toGet(memReqQ[mc]);
                interface response = toPut(memRespQ[mc]);
            endinterface;
        endinterface;
    endfunction
  */ function BC_MC_Client  fn_mkMC_Client (Integer mc);
      return fn_FIFOFs_to_MC_Client (memReqQ [mc], memRespQ [mc],  f_flush_reqs [mc], f_flush_rsps [mc]);
   endfunction
    
    method Action start (BC_AEId fpga_id, BC_Data param_block_addr) if (fsm.done);
        fpgaId <= fpga_id;
        paramPtr <= truncate(param_block_addr) + (extend (fpga_id) << 3);
        engineDoneIdx <= 0;
        doneResetting_pre <= False;
        engineResult <= 0;
	edgePipeStall <= 0;
	worklistStall <= 0;
	engineNodes <= 0;
        engineRetry <= 0;
        watchdog <= 0;
        cycle_counter <= 0; 
	memCounter <= 0;
	set_done <= False;
	rg_numFPGA <= 3;
        for (Integer i = 0; i < 16; i = i + 1) begin
            memCounterGraph[i] <= 0;
            memCounterWorklist[i] <= 0;
            memCounterSSSP[i] <= 0;
        end
        allDone <= True;
        fsm.start;
    endmethod
    
    method Action waitTillDone if(fsm.done);
        $display("[%0d]: mkSSSP[%0d] waitTillDone FINISHED!!!", cur_cycle, fpgaId);
    endmethod
    
    interface mc_ifcs = genWith(fn_mkMC_Client);
endmodule

endpackage
