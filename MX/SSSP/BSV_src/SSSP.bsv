
package SSSP;

import Vector           :: *;
import FIFOF            :: *;
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

import SSSPEngine::*;
import WorklistFIFO::*;

`define NUM_ENGINES 4
`define LG_NUM_ENGINES 2

/*
interface App_Ifc;
    method Action start(BC_AEId fpga_id, BC_Data param_block_addr);
    method Action waitTillDone;
    
    interface Vector#(16, BC_MC_Client) mc_ifcs;
endinterface
*/

interface BC_HW2_IFC;
   method Action start (BC_AEId fpga_id, BC_Data param_block_addr);
   method Action waitTillDone;

   interface Vector #(16, BC_MC_Client) mc_ifcs;
endinterface: BC_HW2_IFC


Integer param_nodePtr = 0;
Integer param_edgePtr = 1;
Integer param_jobsPtr = 2;
Integer param_numJobs = 3;
Integer param_output = 4;
Integer param_status = 5;
Integer param_sentinel = 6;


(* synthesize *)
module mkSSSP(BC_HW2_IFC);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Reg#(BC_Addr) paramPtr <- mkRegU;
    Reg#(BC_Addr) paramNodePtr <- mkRegU;
    Reg#(BC_Addr) paramEdgePtr <- mkRegU;
    Reg#(BC_Addr) paramJobsPtr <- mkRegU;
    Reg#(Bit#(64)) paramNumJobs <- mkRegU;
    Reg#(Bit#(64)) paramOutput  <- mkRegU;
    Reg#(Bit#(64)) paramStatus  <- mkRegU;
    Reg#(Bit#(64)) paramSentinel <- mkRegU;
    
    Reg#(Bit#(`NUM_ENGINES)) engineDoneIdx <- mkRegU; // need +1 for terminating condition
    Vector#(16, FIFOF#(BC_MC_REQ)) memReqQ  <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(BC_MC_RSP)) memRespQ <- replicateM(mkFIFOF);
    Vector #(16, FIFOF #(BC_MC_flush_req)) f_flush_reqs <- replicateM (mkFIFOF);
    Vector #(16, FIFOF #(BC_MC_flush_rsp)) f_flush_rsps <- replicateM (mkFIFOF);
    
    Vector#(`NUM_ENGINES, SSSPEngineIfc) engines <- replicateM(mkSSSPEngine);
    Vector#(`NUM_ENGINES, Reg#(Bit#(64))) engineResults <- replicateM(mkRegU);
    
    Worklist work <- mkWorklistFIFO();
    
    
    /*
    for(Integer mc = 0; mc < 16; mc = mc + 1) begin
        mkConnection();
    end
    */
    
    function Action send_rd_req(BC_Addr base, Integer param_id);
        action
	        let addr = base + fromInteger(param_id * 64);
	        let req = BC_MC_REQ {cmd_sub: REQ_RD, rtnctl: 0, len: BC_8B,
			   vadr: addr, data: ?};
	        memReqQ[param_id].enq(req);
            //$display("%0d: mkSSSP[%0d]: send_rd_eq on channel %0d, addr: %0x", cur_cycle, fpgaId, param_id, addr);
        endaction
    endfunction
    
    function Action send_wr_req(BC_Addr base, Integer param_id, BC_Data x);
        action
	        let addr = base + fromInteger(param_id * 64);
	        let req = BC_MC_REQ {cmd_sub: REQ_WR, rtnctl: 0, len: BC_8B,
			   vadr: addr, data: x};
	        memReqQ[param_id].enq(req);
        endaction
    endfunction
    
    function ActionValue #(BC_Addr) recv_rd_rsp(Integer param_id);
        actionvalue
	        let rsp <- toGet(memRespQ[param_id]).get;
            
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
	        let rsp <- toGet(memRespQ[param_id]).get;
            
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
	       // Send read requests for the parameters for this FPGA (in parallel)
	       action
               $display("%0d: mkSSSP[%0d]: FSM sending...", cur_cycle, fpgaId);
	           send_rd_req (paramPtr, param_nodePtr);
	           send_rd_req (paramPtr, param_edgePtr);
	           send_rd_req (paramPtr, param_jobsPtr);
	           send_rd_req (paramPtr, param_numJobs);
	           send_rd_req (paramPtr, param_output);
	           send_rd_req (paramPtr, param_status);
	           send_rd_req (paramPtr, param_sentinel);
	       endaction

	       // Receive the parameters for this FPGA (read responses, in parallel)
	       action
	           let nodePtr   <- recv_rd_rsp(param_nodePtr);
	           let edgePtr   <- recv_rd_rsp(param_edgePtr);
	           let jobsPtr   <- recv_rd_rsp(param_jobsPtr);
	           let numJobs   <- recv_rd_rsp(param_numJobs);
	           let outputPtr <- recv_rd_rsp(param_output);
	           let status    <- recv_rd_rsp(param_status);
	           let sentinel  <- recv_rd_rsp(param_sentinel);
	           $display ("%0d: mkSSSP [%0d]: params are %0h 0x%0h 0x%0h 0x%0h %0h %0h %0h", cur_cycle, fpgaId, nodePtr, edgePtr, jobsPtr, numJobs, outputPtr, status, sentinel);
               //$display("%0d: mkSSSP returned!", cur_cycle);
	           // Start the N engines
	           for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) action
	               engines[i].start();
                   //engines[i].start(fpgaId, fromInteger(i), nodePtr, edgePtr, jobsPtr, numJobs, outputPtr, status);
	           endaction
	       endaction
           
           // Wait for completion
	       for (engineDoneIdx <= 0; engineDoneIdx < fromInteger(`NUM_ENGINES); engineDoneIdx <= engineDoneIdx + 1) action
	           let result <- engines[engineDoneIdx].result;
	           engineResults[engineDoneIdx] <= result;
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
        fsm.start;
    endmethod
    
    method Action waitTillDone if(fsm.done);
    
    endmethod
    
    interface mc_ifcs = genWith(fn_mkMC_Client);
endmodule

endpackage