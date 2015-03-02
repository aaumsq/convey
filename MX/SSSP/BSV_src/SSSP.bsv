
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
import CreditChannel::*;
import GaloisTypes::*;
`include "GaloisDefs.bsv"

`define NUM_ENGINES 16
`define LG_NUM_ENGINES 4


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
Integer param_status = 5;
Integer param_sentinel = 6;

Integer param_lock = 0;
Integer param_headPtr = 1;
Integer param_tailPtr = 2;
Integer param_wlSize = 3;

(* synthesize *)
module mkSSSP(BC_HW2_IFC);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Reg#(BC_Addr) paramPtr <- mkRegU;
    Reg#(BC_Addr) paramNodePtr <- mkRegU;
    Reg#(BC_Addr) paramEdgePtr <- mkRegU;
    Reg#(BC_Addr) paramJobsPtr <- mkRegU;
    Reg#(BC_Addr) paramMetaPtr <- mkRegU;
    Reg#(BC_Addr) paramOutputPtr  <- mkRegU;
    Reg#(BC_Addr) paramStatus  <- mkRegU;
    Reg#(BC_Addr) paramSentinel <- mkRegU;
    
    Reg#(Bit#(`NUM_ENGINES)) engineDoneIdx <- mkRegU; // need +1 for terminating condition
    Reg#(Bool) allDone <- mkRegU;
    Reg#(Bit#(10)) numAllDones <- mkRegU;
    Reg#(Bool) doneResetting <- mkReg(False);
    
    Clock clk <- exposeCurrentClock;
    Reset rst <- exposeCurrentReset;
    
    Vector#(16, MakeResetIfc) engineRsts <- replicateM(mkReset(1, False, clk));
    MakeResetIfc graphRst<- mkReset(1, False, clk);
    MakeResetIfc worklistRst <- mkReset(1, False, clk);
    
    Vector#(16, FIFOF#(BC_MC_REQ)) memReqQ  <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(BC_MC_RSP)) memRespQ <- replicateM(mkFIFOF);
    Vector #(16, FIFOF #(BC_MC_flush_req)) f_flush_reqs <- replicateM (mkFIFOF);
    Vector #(16, FIFOF #(BC_MC_flush_rsp)) f_flush_rsps <- replicateM (mkFIFOF);

    Vector#(16, FIFOF#(BC_MC_REQ)) ssspOutQs <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(BC_MC_RSP)) ssspInQs  <- replicateM(mkFIFOF);
    
    Vector#(`NUM_ENGINES, Engine) engines;
    for(Integer i = 0; i < `NUM_ENGINES; i=i+1) begin
        engines[i] <- mkSSSPEngine(reset_by engineRsts[i].new_rst);
    end
    Vector#(`NUM_ENGINES, Channel#(GraphReq, GraphResp)) engineGraphChannels <- replicateM(mkCreditChannel(16));
    
    Vector#(`NUM_ENGINES, FIFOF#(BC_MC_REQ)) engineOutQs <- replicateM(mkFIFOF);
    Vector#(`NUM_ENGINES, FIFOF#(BC_MC_RSP)) engineInQs  <- replicateM(mkFIFOF);
    Vector#(`NUM_ENGINES, Reg#(Bit#(64))) engineResults <- replicateM(mkRegU);
    
    Worklist worklist <- mkWorklistFIFO(reset_by worklistRst.new_rst);
    Vector#(16, FIFOF#(BC_MC_REQ)) worklistOutQs <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(BC_MC_RSP)) worklistInQs  <- replicateM(mkFIFOF);
    
    GraphEngine graph <- mkGraphEngine(reset_by graphRst.new_rst);
    Vector#(16, FIFOF#(BC_MC_REQ)) graphOutQs <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(BC_MC_RSP)) graphInQs  <- replicateM(mkFIFOF);
    
    
    for(Integer i = 0; i < 16; i = i + 1) begin

        mkConnection(engines[i].workOut, worklist.enq[i]);
        mkConnection(worklist.deq[i], engines[i].workIn);
        
        mkConnection(worklist.memReq[i], toPut(worklistOutQs[i]));
        mkConnection(toGet(worklistInQs[i]), worklist.memResp[i]);
        
        mkConnection(engines[i].graphReq, engineGraphChannels[i].reqToChan);
        mkConnection(engineGraphChannels[i].reqFromChan, graph.req[i]);
        mkConnection(graph.resp[i], engineGraphChannels[i].respToChan);
        mkConnection(engineGraphChannels[i].respFromChan, engines[i].graphResp);
        //mkConnection(engines[i].graphReq, graph.req[i]);
        //mkConnection(graph.resp[i], engines[i].graphResp);
        
        mkConnection(graph.memReq[i], toPut(graphOutQs[i]));
        mkConnection(toGet(graphInQs[i]), graph.memResp[i]);
        
        rule toMem(doneResetting && (engineOutQs[i].notEmpty || worklistOutQs[i].notEmpty || graphOutQs[i].notEmpty || ssspOutQs[i].notEmpty));
            if(graphOutQs[i].notEmpty) begin
                BC_MC_REQ req = graphOutQs[i].first();
                graphOutQs[i].deq();
                memReqQ[i].enq(req);
                //$display("toMem Graph routing to mem %0d ", i, fshow(req));
            end
            else if(engineOutQs[i].notEmpty) begin
                BC_MC_REQ req = engineOutQs[i].first();
                engineOutQs[i].deq();
                memReqQ[i].enq(req);
                //$display("toMem Engine routing to mem %0d ", i, fshow(req));
            end
            else if(worklistOutQs[i].notEmpty) begin
                BC_MC_REQ req = worklistOutQs[i].first();
                worklistOutQs[i].deq();
                memReqQ[i].enq(req);
                //$display("toMem WorkList routing to mem %0d ", i, fshow(req));
            end
            else if(ssspOutQs[i].notEmpty) begin
                BC_MC_REQ req = ssspOutQs[i].first();
                ssspOutQs[i].deq();
                memReqQ[i].enq(req);
                $display("toMem SSSP routing to mem %0d ", i, fshow(req));
            end 
        endrule
        
        // rtnctl: 0 means it's for mkSSSP
        rule fromMem(doneResetting);
            BC_MC_RSP resp = memRespQ[i].first();
            memRespQ[i].deq();
            
            GaloisAddress gaddr = unpack(resp.rtnctl);
            $display("Received packed gaddr %d", gaddr);
            if(gaddr.mod == MK_ENGINE) begin
                //$display("fromMem packet routing to Engine %0d", i);
                engineInQs[i].enq(resp);
            end
            else if(gaddr.mod == MK_WORKLIST) begin
                //$display("fromMem packet routing to Worklist %0d", i);
                worklistInQs[i].enq(resp);
            end
            else if(gaddr.mod == MK_GRAPH) begin
                graphInQs[i].enq(resp);
            end
            else if(gaddr.mod == MK_SSSP) begin
                //$display("fromMem packet routing to SSSP %0d", i);
                ssspInQs[i].enq(resp);
            end
            else begin
                $display("ERROR: fromMem packet dest unknown: ", fshow(resp));
            end
        endrule
    end
    
    function Action send_rd_req(BC_Addr base, Integer param_id);
        action
	        let addr = base + fromInteger(param_id * 64);
	        let req = BC_MC_REQ {cmd_sub: REQ_RD, rtnctl: pack(GaloisAddress{mod: MK_SSSP, addr: 0}), len: BC_8B,
			   vadr: addr, data: ?};
	        ssspOutQs[param_id].enq(req);
            //$display("%0d: mkSSSP[%0d]: send_rd_eq on channel %0d, addr: %0x", cur_cycle, fpgaId, param_id, addr);
        endaction
    endfunction
    
    function Action send_wr_req(BC_Addr base, Integer param_id, BC_Data x);
        action
	        let addr = base + fromInteger(param_id * 64);
	        let req = BC_MC_REQ {cmd_sub: REQ_WR, rtnctl: pack(GaloisAddress{mod: MK_SSSP, addr: 0}), len: BC_8B,
			   vadr: addr, data: x};
	        ssspOutQs[param_id].enq(req);
        endaction
    endfunction
    
    function ActionValue #(BC_Addr) recv_rd_rsp(Integer param_id);
        actionvalue
	        let rsp <- toGet(ssspInQs[param_id]).get;
            
            // Assertion: the only responses in f_rsps[] should be RSP_RD_DATA responses
	        // for the parameter read requests
	        if (rsp.cmd != RSP_RD_DATA) begin
	            $display("INTERNAL ERROR: mkSSSP: memory response for parameter-read is of wrong RSP_TYPE: ", fshow(rsp.cmd));
	            $display("    Response is: ", fshow(rsp));
	            $finish(1);
	        end
            
	        return truncate(rsp.data);
        endactionvalue
    endfunction
    
    function Action recv_wr_rsp(Integer param_id);
        action
	        let rsp <- toGet(ssspInQs[param_id]).get;
            
	        // Assertion: the only responses in f_rsps[] should be RSP_WR_CMP responses
	        // for the paramter write requests
	        if (rsp.cmd != RSP_WR_CMP) begin
	            $display("INTERNAL ERROR: mkSSSP: memory response for parameter-write is of wrong RSP_TYPE: ", fshow(rsp.cmd));
	            $display("    Response is: ", fshow(rsp));
	            $finish(1);
	        end
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
	           send_rd_req (paramPtr, param_status);
	           send_rd_req (paramPtr, param_sentinel);
	       endaction

	       // Receive the parameters for this FPGA (read responses, in parallel)
	       action
	           let nodePtr   <- recv_rd_rsp(param_nodePtr);
               paramNodePtr <= nodePtr;
           endaction
           action
	           let edgePtr   <- recv_rd_rsp(param_edgePtr);
               paramEdgePtr <= edgePtr;
           endaction
           action
	           let jobsPtr   <- recv_rd_rsp(param_jobsPtr);
               paramJobsPtr <= jobsPtr;
           endaction
           action
	           let metaPtr <- recv_rd_rsp(param_metaPtr);
               paramMetaPtr <= metaPtr;
           endaction
           action
	           let outputPtr <- recv_rd_rsp(param_output);
               paramOutputPtr <= outputPtr;
           endaction
           action
	           let status    <- recv_rd_rsp(param_status);
               paramStatus <= status;
           endaction
           action
	           let sentinel  <- recv_rd_rsp(param_sentinel);
               paramSentinel <= sentinel;
           endaction
           action
               $display ("%0d: mkSSSP [%0d]: params are %0h 0x%0h 0x%0h 0x%0h %0h %0h %0h", cur_cycle, fpgaId, paramNodePtr, paramEdgePtr, paramJobsPtr, paramMetaPtr, paramOutputPtr, paramStatus, paramSentinel);
           endaction
           
           // Read metadata
           action
               Bit#(32) rtn = pack(GaloisAddress{mod: MK_SSSP, addr: 0});
               ssspOutQs[0].enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: rtn, len: BC_8B, vadr: paramMetaPtr+fromInteger(param_lock*8), data: ?});
	           ssspOutQs[1].enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: rtn, len: BC_8B, vadr: paramMetaPtr+fromInteger(param_headPtr*8), data: ?});
	           ssspOutQs[2].enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: rtn, len: BC_8B, vadr: paramMetaPtr+fromInteger(param_tailPtr*8), data: ?});
	           ssspOutQs[3].enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: rtn, len: BC_8B, vadr: paramMetaPtr+fromInteger(param_wlSize*8), data: ?});
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
               
               worklist.init(fpgaId, lockLoc, headPtrLoc, tailPtrLoc, wlSize, paramJobsPtr);
           endaction
           
           action
               graph.init(fpgaId, paramNodePtr, paramEdgePtr);
           endaction
       
           action
               for(Integer i = 0; i < `NUM_ENGINES; i=i+1) action
                   engineGraphChannels[i].init();
               endaction
           endaction
           action
               // Start the N engines
	           for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) action
	               engines[i].init(fpgaId);
	           endaction
           endaction
           // Wait for completion
           allDone <= False;
           numAllDones <= 0;
           while(numAllDones < 10) seq
               $display("%0d: SSSP[%0d]: Checking allDones %0d...", cur_cycle, fpgaId, numAllDones);
               allDone <= True;
               action
                   if(!worklist.isDone) begin
                     allDone <= False;
                   end
               endaction
	           for (engineDoneIdx <= 0; engineDoneIdx < fromInteger(`NUM_ENGINES); engineDoneIdx <= engineDoneIdx + 1) action
                   if(!engines[engineDoneIdx].isDone) begin
                     allDone <= False;
                     $display("%0d: SSSP[%0d]: Engine %0d not done!", cur_cycle, fpgaId, engineDoneIdx);
                   end
	           endaction
               if(allDone)
                   numAllDones <= numAllDones + 1;
           endseq

           $display("%0d: SSSP[%0d]: All Done!", cur_cycle, fpgaId);
           //let result <- engines[engineDoneIdx].result;
	       //engineResults[engineDoneIdx] <= result;

           
           
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
        fsm.start;
    endmethod
    
    method Action waitTillDone if(fsm.done);
        $display("[%0d]: mkSSSP[%0d] waitTillDone FINISHED!!!", cur_cycle, fpgaId);
    endmethod
    
    interface mc_ifcs = genWith(fn_mkMC_Client);
endmodule

endpackage