
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

`define NUM_ENGINES 16
`define LG_NUM_ENGINES 4
`define WATCHDOG_TIMEOUT 5000000000
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
Integer param_headPtr = 1;
Integer param_tailPtr = 2;
Integer param_wlSize = 3;

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
    
    Reg#(Bit#(`NUM_ENGINES)) engineDoneIdx <- mkRegU; // need +1 for terminating condition
    Reg#(Bool) done <- mkRegU;
    Reg#(Bit#(4)) numDones <- mkRegU;
    Reg#(Bool) allDone <- mkRegU;
    Reg#(Bit#(4)) numAllDones <- mkRegU;
    Reg#(Bool) doneResetting <- mkReg(False);
    Reg#(Bool) incremented <- mkRegU;
    Reg#(Bit#(64)) watchdog <- mkRegU;
    
    Clock clk <- exposeCurrentClock;
    Reset rst <- exposeCurrentReset;
    
    Vector#(16, MakeResetIfc) engineRsts <- replicateM(mkReset(1, False, clk));
    MakeResetIfc graphRst <- mkReset(1, False, clk);
    MakeResetIfc worklistRst <- mkReset(1, False, clk);
    
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
    
    Vector#(`NUM_ENGINES, FIFOF#(MemReq)) engineOutQs <- replicateM(mkFIFOF);
    Vector#(`NUM_ENGINES, FIFOF#(MemResp)) engineInQs  <- replicateM(mkFIFOF);
    Vector#(`NUM_ENGINES, Reg#(Bit#(64))) engineResults <- replicateM(mkRegU);
    
    Worklist worklist <- mkWorklistFIFO(reset_by worklistRst.new_rst);
    Vector#(16, FIFOF#(MemReq)) worklistOutQs <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemResp)) worklistInQs  <- replicateM(mkFIFOF);
    
    GraphEngine graph <- mkGraphEngine(reset_by graphRst.new_rst);
    Vector#(16, FIFOF#(MemReq)) graphOutQs <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemResp)) graphInQs  <- replicateM(mkFIFOF);
    
    rule watchdogInc;
        watchdog <= watchdog + 1;
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
    
    for(Integer i = 0; i < 16; i = i + 1) begin

        mkConnection(engines[i].workOut, worklist.enq[i]);
        mkConnection(worklist.deq[i], engines[i].workIn);
        
        mkConnection(worklist.memReq[i], toPut(worklistOutQs[i]));
        mkConnection(toGet(worklistInQs[i]), worklist.memResp[i]);
        
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
        //mkConnection(engines[i].graphReq, graph.req[i]);
        //mkConnection(graph.resp[i], engines[i].graphResp);
        
        mkConnection(graph.memReq[i], toPut(graphOutQs[i]));
        mkConnection(toGet(graphInQs[i]), graph.memResp[i]);
        
        rule toMem(doneResetting && (engineOutQs[i].notEmpty || worklistOutQs[i].notEmpty || graphOutQs[i].notEmpty || ssspOutQs[i].notEmpty));
            if(graphOutQs[i].notEmpty) begin
                MemReq req = graphOutQs[i].first();
                graphOutQs[i].deq();
                memReqQ[i].enq(memReqToBC(req));
                //$display("toMem Graph routing to mem %0d ", i, fshow(req));
            end
            else if(engineOutQs[i].notEmpty) begin
                MemReq req = engineOutQs[i].first();
                engineOutQs[i].deq();
                memReqQ[i].enq(memReqToBC(req));
                //$display("toMem Engine routing to mem %0d ", i, fshow(req));
            end
            else if(worklistOutQs[i].notEmpty) begin
                MemReq req = worklistOutQs[i].first();
                worklistOutQs[i].deq();
                memReqQ[i].enq(memReqToBC(req));
                //$display("toMem WorkList routing to mem %0d ", i, fshow(req));
            end
            else if(ssspOutQs[i].notEmpty) begin
                MemReq req = ssspOutQs[i].first();
                ssspOutQs[i].deq();
                memReqQ[i].enq(memReqToBC(req));
                //$display("toMem SSSP routing to mem %0d ", i, fshow(req));
            end 
        endrule
        
        // rtnctl: 0 means it's for mkSSSP
        rule fromMem(doneResetting);
            BC_MC_RSP resp = memRespQ[i].first();
            memRespQ[i].deq();
            
            GaloisAddress gaddr = unpack(truncate(resp.rtnctl));
            //$display("Received packed gaddr %d", gaddr);
            if(gaddr.mod == MK_ENGINE) begin
                //$display("fromMem packet routing to Engine %0d", i);
                engineInQs[i].enq(bcToMemResp(resp));
            end
            else if(gaddr.mod == MK_WORKLIST) begin
                //$display("fromMem packet routing to Worklist %0d", i);
                worklistInQs[i].enq(bcToMemResp(resp));
            end
            else if(gaddr.mod == MK_GRAPH) begin
                graphInQs[i].enq(bcToMemResp(resp));
            end
            else if(gaddr.mod == MK_SSSP) begin
                //$display("fromMem packet routing to SSSP %0d", i);
                ssspInQs[i].enq(bcToMemResp(resp));
            end
            else begin
                $display("ERROR: fromMem packet dest unknown: ", fshow(resp));
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
               for(Integer i = 0; i < 16; i=i+1) action
                   engineRsts[i].assertReset();
               endaction
               graphRst.assertReset();
               worklistRst.assertReset();
           endaction
           action
               noAction;
           endaction
           
           action
               doneResetting <= True;
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
               ssspOutQs[1].enq(MemRead64{addr: paramMetaPtr+fromInteger(param_headPtr*8), gaddr: rtn});
               ssspOutQs[2].enq(MemRead64{addr: paramMetaPtr+fromInteger(param_tailPtr*8), gaddr: rtn});
               ssspOutQs[3].enq(MemRead64{addr: paramMetaPtr+fromInteger(param_wlSize*8), gaddr: rtn});
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
               
               worklist.init(fpgaId, lockLoc, headPtrLoc, tailPtrLoc, truncate(wlSize), paramJobsPtr);
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
           endaction
           
           while(numAllDones < 3 && watchdog < `WATCHDOG_TIMEOUT) seq
               numDones <= 0;
               //$display("mkSSSP[%0d]: Checking local dones..., numAllDones = %d", fpgaId, numAllDones);
               while(numDones < 10 && watchdog < `WATCHDOG_TIMEOUT) seq
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
                         //if(!done) $display("%0d: SSSP[%0d]: Engine %0d not done!", cur_cycle, fpgaId, engineDoneIdx);
                     end
	               endaction
                   action
                     if(done)
                         numDones <= numDones + 1;
                     else begin
                         numDones <= 0;
                         numAllDones <= 1;
                         //$display("mkSSSP[%0d]: RESETTING ALL DONE", fpgaId);
                     end
                   endaction
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
       
                   if((d0 >= numAllDones) && (d1 >= numAllDones) && (d2 >= numAllDones) && (d3 >= numAllDones)) begin
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
                              
           $display("%0d: SSSP[%0d]: All Done!", cur_cycle, fpgaId);
           action
               for(Integer i = 0; i < 16; i=i+1) action
                   let result <- engines[i].result;
	               engineResults[i] <= result;
                   $display("Engine[%0d][%0d] edges fetched = %0d", fpgaId, i, result);
               endaction
           endaction
           
	       // Write final (per-FPGA) sum back to param block, and drain the response
	       send_wr_req (paramPtr, param_output, 64'hCAFEBABE_BEEFBEEF);
	       recv_wr_rsp (param_output);
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
        doneResetting <= False;
        watchdog <= 0;
        fsm.start;
    endmethod
    
    method Action waitTillDone if(fsm.done);
        $display("[%0d]: mkSSSP[%0d] waitTillDone FINISHED!!!", cur_cycle, fpgaId);
    endmethod
    
    interface mc_ifcs = genWith(fn_mkMC_Client);
endmodule

endpackage