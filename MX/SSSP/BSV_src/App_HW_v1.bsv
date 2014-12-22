// Copyright (c) 2012-2015 Bluespec, Inc.  All Rights Reserved.
//
// Author: Rishiyur S. Nikhil

package App_HW;

// ================================================================
// This is Version 1 of the Vector-Add example for the Bluespec-Convey
// library (BClib).  Here, we assume that all three vectors are
// identically aligned w.r.t. the MCs.  Hence, memory requests and
// responses go to statically specified MCs.

// This version assumes out-of-order memory read-responses
// When compiled with the Convey PDK, therefore, one can use:
//     MC_READ_ORDER = 0    (omits PDK re-ordering logic)

// ================================================================
// BSV Library imports

import Vector           :: *;
import FIFOF            :: *;
import GetPut           :: *;
import ClientServer     :: *;
import Connectable      :: *;
import StmtFSM          :: *;

// ----------------
// BC library imports

import BC_Utils           :: *;
import BC_HW_IFC          :: *;
import BC_Transactors     :: *;

// ================================================================
// Application-specific aeg count

Integer aeg_cnt = 1;    // We use only 1 AEG register!

// ================================================================
// Top-level of the app (for 1 FPGA)

(* synthesize *)
module mkApp_HW (BC_HW_IFC);

   // ----------------------------------------------------------------
   // Instantiate a null management transactor

   match { .top_management_ifc, .management_app_ifc } <- mkBC_Management_Null_Transactor;

   // Get the fpga number (aeid) from the transactor
   BC_AEId  aeid = management_app_ifc.aeid;

   // ----------------------------------------------------------------
   // Instantiate Simple Decode/Dispatch transactor with aeg_cnt registers

   match { .top_dispatch_ifc, .simple_dispatch_app_ifc } <- mkSimple_Dispatch (fromInteger (aeg_cnt - 1));

   rule rl_propagate_aeid;
      simple_dispatch_app_ifc.set_aeid (aeid);
   endrule

   // ----------------------------------------------------------------
   // Instantiate Reordering transactors for MCs

   match { .v16_mc_clients, .v16_mc_servers } <- mkBC_Vec_MC_Port_Ordering_Transactors;

   // ----------------------------------------------------------------
   // Instantiate the App and connect to mem transactors
   
   BC_HW2_IFC hw2 <- mkApp_HW2;
   mkConnection (hw2.mc_ifcs, v16_mc_servers);

   // ----------------------------------------------------------------
   // Main behavior

   mkAutoFSM (
      seq
	 while (True) seq
	    $display ("%0d: mkApp_HW [%0d]: awaiting caep instruction", cur_cycle, aeid);
	    action
	       // The following just waits for a caep instruction; we don't have to actually
	       // examine it since we're implementing only one op (CAEP00) with no immediates etc.
	       let inst <- simple_dispatch_app_ifc.get_caep_inst.get;
	    endaction
	    action
	       // Read the arg in AEG 0 and start hw2 with this arg
	       BC_Data arg = simple_dispatch_app_ifc.rd_aeg (0);
	       $display ("%0d: mkApp_HW [%0d]: Starting HW2 with arg 0x%0h", cur_cycle, aeid, arg);
	       hw2.start (aeid, arg);
	    endaction
	    hw2.waitTillDone;
	    simple_dispatch_app_ifc.put_caep_result.put (0); // signal caep inst completion
	 endseq
      endseq
      );

   // ================================================================
   // INTERFACE

   interface mc_ifcs        = v16_mc_clients;
   interface dispatch_ifc   = top_dispatch_ifc;
   interface management_ifc = top_management_ifc;
endmodule: mkApp_HW

// ================================================================
// HW module with simplified API

// ----------------

interface BC_HW2_IFC;
   method Action start (BC_AEId fpga_id, BC_Data param_block_addr);
   method Action waitTillDone;

   interface Vector #(16, BC_MC_Client) mc_ifcs;
endinterface: BC_HW2_IFC

// ----------------
// Position of parameters in param block

Integer param_VSIZE       = 0;
Integer param_VIN1_P      = 1;
Integer param_VIN2_P      = 2;
Integer param_VOUT_P      = 3;
Integer param_PARTIAL_SUM = 4;

// ----------------

(* synthesize *)
module mkApp_HW2 (BC_HW2_IFC);
   Reg #(BC_AEId)         rg_fpga_id          <- mkRegU;
   Reg #(BC_Addr)         rg_param_block_addr <- mkRegU;
   Reg #(BC_Data)         rg_partial_sum      <- mkRegU;

   // ----------------------------------------------------------------
   // MC FIFOs

   Vector #(16, FIFOF #(BC_MC_REQ))       f_reqs       <- replicateM (mkFIFOF);
   Vector #(16, FIFOF #(BC_MC_RSP))       f_rsps       <- replicateM (mkFIFOF);
   Vector #(16, FIFOF #(BC_MC_flush_req)) f_flush_reqs <- replicateM (mkFIFOF);
   Vector #(16, FIFOF #(BC_MC_flush_rsp)) f_flush_rsps <- replicateM (mkFIFOF);

   // ----------------------------------------------------------------
   // Instantiate 8 vadders, and connect them to the MC ports

   Vector #(8, ChanVadd_IFC) v_chanvadds <- replicateM (mkChanVadd);

   for (Integer mc = 0; mc < 8; mc = mc + 1) begin
      let mc_server_pair_mc = tuple2 (fn_FIFOFs_to_MC_Server (f_reqs [2 * mc],
							      f_rsps [2 * mc],
							      f_flush_reqs [2 * mc],
							      f_flush_rsps [2 *mc]),
				      fn_FIFOFs_to_MC_Server (f_reqs [2 * mc + 1],
							      f_rsps [2 * mc + 1],
							      f_flush_reqs [2 * mc + 1],
							      f_flush_rsps [2 * mc + 1]));
      mkConnection (v_chanvadds[mc].mc_client_pair, mc_server_pair_mc);
   end

   // ----------------------------------------------------------------
   // The following are used for reading/writing from/to the parameter block
   // Note, they share the same req/rsp fifos with the mkChanVadd modules,
   // but there is no confusion because they are used at disjoint phases
   // of the FSM below.

   function Action send_rd_req (BC_Addr base, Integer param_id);
      action
	 let addr = base + fromInteger (param_id * 64);
	 let req = BC_MC_REQ {cmd_sub: REQ_RD, rtnctl: 0, len: BC_8B,
			      vadr: addr, data: ?};
	 f_reqs [param_id * 2].enq (req);
      endaction
   endfunction

   function Action send_wr_req (BC_Addr base, Integer param_id, BC_Data x);
      action
	 let addr = base + fromInteger (param_id * 64);
	 let req = BC_MC_REQ {cmd_sub: REQ_WR, rtnctl: 0, len: BC_8B,
			      vadr: addr, data: x};
	 f_reqs [param_id * 2].enq (req);
      endaction
   endfunction

   function ActionValue #(BC_Addr) recv_rd_rsp (Integer param_id);
      actionvalue
	 let rsp <- toGet (f_rsps [param_id * 2]).get;

	 // Assertion: the only responses in f_rsps[] should be RSP_RD_DATA responses
	 // for the parameter read requests
	 if (rsp.cmd != RSP_RD_DATA) begin
	    $display ("INTERNAL ERROR: mkApp_HW2: memory response for parameter-read is of wrong RSP_TYPE",
		      fshow (rsp.cmd));
	    $display ("    Response is: ", fshow (rsp));
	    $finish (1);
	 end

	 return truncate (rsp.data);
      endactionvalue
   endfunction

   function Action recv_wr_rsp (Integer param_id);
      action
	 let rsp <- toGet (f_rsps [param_id * 2]).get;

	 // Assertion: the only responses in f_rsps[] should be RSP_WR_CMP responses
	 // for the parameter write requests
	 if (rsp.cmd != RSP_WR_CMP) begin
	    $display ("INTERNAL ERROR: mkApp_HW2: memory response for parameter-write is of wrong RSP_TYPE",
		      fshow (rsp.cmd));
	    $display ("    Response is: ", fshow (rsp));
	    $finish (1);
	 end
      endaction
   endfunction

   // ----------------------------------------------------------------
   // MAIN BEHAVIOR 

   Reg #(UInt #(4)) rg_mc <- mkRegU;

   Reg #(BC_Addr) rg_vsize  <- mkRegU;
   Reg #(BC_Addr) rg_vin1_p <- mkRegU;
   Reg #(BC_Addr) rg_vin2_p <- mkRegU;
   Reg #(BC_Addr) rg_vout_p <- mkRegU;

   let fsm <- mkFSM (
      seq
	 // Send read requests for the parameters for this FPGA (in parallel)
	 action
	    send_rd_req (rg_param_block_addr, param_VSIZE);
	    send_rd_req (rg_param_block_addr, param_VIN1_P);
	    send_rd_req (rg_param_block_addr, param_VIN2_P);
	    send_rd_req (rg_param_block_addr, param_VOUT_P);
	 endaction
	 // Receive the parameters for this FPGA (read responses, in parallel)
	 // and start the channel adders
	 action
	    let vsize   <- recv_rd_rsp (param_VSIZE);
	    let vin1_p  <- recv_rd_rsp (param_VIN1_P);
	    let vin2_p  <- recv_rd_rsp (param_VIN2_P);
	    let vout_p  <- recv_rd_rsp (param_VOUT_P);
	    $display ("%0d: mkApp_HW2 [%0d]: params are %0d 0x%0h 0x%0h 0x%0h; starting per-MC vadds",
		      cur_cycle, rg_fpga_id, vsize, vin1_p, vin2_p, vout_p);
	    for (Integer mc = 0; mc < 8; mc = mc + 1)
	       v_chanvadds [mc].start (rg_fpga_id, fromInteger (mc),
				       bc_base_addr_for_chan (mc, vin1_p), vin1_p + (vsize << 3),
				       bc_base_addr_for_chan (mc, vin2_p), vin2_p + (vsize << 3),
				       bc_base_addr_for_chan (mc, vout_p), vout_p + (vsize << 3));
	    rg_partial_sum <= 0;
	 endaction
	 // Collect per-channel partial sums, accumulate into full (per-FPGA) sum
	 for (rg_mc <= 0; rg_mc < 8; rg_mc <= rg_mc + 1) action
	    let chan_sum <- v_chanvadds [rg_mc].result;
	    rg_partial_sum <= rg_partial_sum + chan_sum;
	 endaction
	 // Write final (per-FPGA) sum back to param block, and drain response
	 send_wr_req (rg_param_block_addr, param_PARTIAL_SUM, rg_partial_sum);
	 recv_wr_rsp (param_PARTIAL_SUM);
      endseq
      );

   // ----------------------------------------------------------------
   // INTERFACE

   function BC_MC_Client  fn_mkMC_Client (Integer mc);
      return fn_FIFOFs_to_MC_Client (f_reqs [mc], f_rsps [mc], f_flush_reqs [mc], f_flush_rsps [mc]);
   endfunction

   method Action start (BC_AEId fpga_id, BC_Data param_block_addr) if (fsm.done);
      rg_fpga_id          <= fpga_id;
      rg_param_block_addr <= truncate (param_block_addr) + (extend (fpga_id) << 3);
      fsm.start;
   endmethod

   method Action waitTillDone() if (fsm.done);
      noAction;
   endmethod

   interface mc_ifcs = genWith (fn_mkMC_Client);
endmodule

// ================================================================
// The foll. module is instantiated 8 times, once for each MC (mem controller/channel)
// It performs the vector-add for the slices of the vectors that are in MC 'chan'
// Reads from input vector vin1 are done on the even port
// Reads from input vector vin2 are done on the odd  port
// Writes to output vector vout are alternated on the even and odd ports

interface ChanVadd_IFC;
   method Action start (BC_AEId aeid, BC_MC chan,                    // for $displays only
			BC_Addr rd_base_1,  BC_Addr rd_limit_1,
			BC_Addr rd_base_2,  BC_Addr rd_limit_2,
			BC_Addr wr_base,    BC_Addr wr_limit);
   method ActionValue #(BC_Data) result ();    // chan partial sum

   interface Tuple2 #(BC_MC_Client, BC_MC_Client)  mc_client_pair;
endinterface

(* synthesize *)
module mkChanVadd (ChanVadd_IFC);
   // These are only needed for $displays
   Reg #(BC_AEId)  rg_aeid <- mkRegU;
   Reg #(BC_Data)  rg_partial_sum <- mkRegU;

   // The purpose of rg_running is to disable the put_rd_rsp methods while the
   // parent is still reading the initial params from mem; i.e., so that this method
   // doesn't compete for read responses until we've started the actual computation.
   Reg #(Bool)            rg_running     <- mkReg (False);

   // Input vector 1
   Reg #(BC_Addr)         rg_rd_base_1   <- mkRegU;
   Reg #(BC_Addr)         rg_rd_limit_1  <- mkRegU;
   Reg #(Bool)            rg_rd_active_1 <- mkReg (False);

   // Input vector 2
   Reg #(BC_Addr)         rg_rd_base_2   <- mkRegU;
   Reg #(BC_Addr)         rg_rd_limit_2  <- mkRegU;
   Reg #(Bool)            rg_rd_active_2 <- mkReg (False);

   // Output vector
   Reg #(BC_Addr)         rg_wr_base     <- mkRegU;
   Reg #(BC_Addr)         rg_wr_limit    <- mkRegU;
   Reg #(Bool)            rg_wr_active   <- mkReg (False);
   Reg #(Bit #(16))       rg_wr_rsp_1_drain_count <- mkRegU;
   Reg #(Bit #(16))       rg_wr_rsp_2_drain_count <- mkRegU;

   // Mem req/rsp fifos
   FIFOF #(BC_MC_REQ)  fifo_reqs_1    <- mkFIFOF;
   FIFOF #(BC_MC_RSP)  fifo_rsps_1 <- mkFIFOF;

   FIFOF #(BC_MC_REQ)  fifo_reqs_2    <- mkFIFOF;
   FIFOF #(BC_MC_RSP)  fifo_rsps_2 <- mkFIFOF;

   // ----------------------------------------------------------------
   // Generate read request from vin1

   rule rl_rd_gen_1 (rg_rd_active_1);
      let req  = BC_MC_REQ {cmd_sub: REQ_RD, rtnctl: 0, len:BC_8B, vadr: rg_rd_base_1, data:?};
      fifo_reqs_1.enq (req);

      let next_base = bc_next_addr_8B_in_chan (rg_rd_base_1);
      rg_rd_base_1 <= next_base;
      rg_rd_active_1 <= (next_base < rg_rd_limit_1);
   endrule

   // ----------------------------------------------------------------
   // Generate read request from vin2

   rule rl_rd_gen_2 (rg_rd_active_2);
      let req  = BC_MC_REQ {cmd_sub: REQ_RD, rtnctl: 0, len:BC_8B, vadr: rg_rd_base_2, data:?};
      fifo_reqs_2.enq (req);

      let next_base = bc_next_addr_8B_in_chan (rg_rd_base_2);
      rg_rd_base_2 <= next_base;
      rg_rd_active_2 <= (next_base < rg_rd_limit_2);
   endrule

   // ----------------------------------------------------------------
   // Receive a 64b word from each of vin1 and vin2, write sum to vout,
   // and accumulate into partial sum

   rule rl_sum_and_gen_wr_reqs (rg_wr_active
				&& (fifo_rsps_1.first.cmd == RSP_RD_DATA)
				&& (fifo_rsps_2.first.cmd == RSP_RD_DATA));
      let x1 = fifo_rsps_1.first.data;  fifo_rsps_1.deq;    // from vin1
      let x2 = fifo_rsps_2.first.data;  fifo_rsps_2.deq;    // from vin2
      let x3 = x1 + x2;

      // Write x3 to vout. Alternate between even and odd ports.
      let wr_req       = BC_MC_REQ {cmd_sub: REQ_WR, rtnctl: 0, len:BC_8B, vadr: rg_wr_base, data: x3};
      if (rg_wr_base [3] == 1'b0) begin
	 fifo_reqs_1.enq (wr_req);
	 rg_wr_rsp_1_drain_count <= rg_wr_rsp_1_drain_count + 1;
      end
      else begin
	 fifo_reqs_2.enq (wr_req);
	 rg_wr_rsp_2_drain_count <= rg_wr_rsp_2_drain_count + 1;
      end

      // Accumulate x3 in partial sum
      rg_partial_sum <= rg_partial_sum + x3;
      let next_base = bc_next_addr_8B_in_chan (rg_wr_base);
      rg_wr_base <= next_base;
      rg_wr_active <= (next_base < rg_wr_limit);
   endrule

   (* descending_urgency = "rl_sum_and_gen_wr_reqs, rl_wr_rsp_1_drain" *)
   rule rl_wr_rsp_1_drain ((rg_wr_rsp_1_drain_count != 0) && (fifo_rsps_1.first.cmd == RSP_WR_CMP));
      fifo_rsps_1.deq;
      rg_wr_rsp_1_drain_count <= rg_wr_rsp_1_drain_count - 1;
   endrule

   (* descending_urgency = "rl_sum_and_gen_wr_reqs, rl_wr_rsp_2_drain" *)
   rule rl_wr_rsp_2_drain ((rg_wr_rsp_2_drain_count != 0) && (fifo_rsps_2.first.cmd == RSP_WR_CMP));
      fifo_rsps_2.deq;
      rg_wr_rsp_2_drain_count <= rg_wr_rsp_2_drain_count - 1;
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   function BC_MC_Client  mkMC_Client (Reg #(Bool)         rg_rd_active,
				       Reg #(BC_Addr)      rg_rd_base,
				       Reg #(BC_Addr)      rg_rd_limit,
				       FIFOF #(BC_MC_REQ)  fifo_reqs,
				       FIFOF #(BC_MC_RSP)  fifo_rsps);
      return interface BC_MC_Client;
		interface Client req_rsp;
		   interface Get request = toGet (fifo_reqs);
		   interface Put response;
		      method Action put (BC_MC_RSP rsp) if (rg_running);
			 fifo_rsps.enq (rsp);
		      endmethod
		   endinterface
		endinterface
		interface Client flush;
		   interface Get request  = getstub;
		   interface Put response = putstub;
		endinterface
	     endinterface;
   endfunction

   let ifc_e = mkMC_Client (rg_rd_active_1, rg_rd_base_1, rg_rd_limit_1, fifo_reqs_1, fifo_rsps_1);

   let ifc_o = mkMC_Client (rg_rd_active_2, rg_rd_base_2, rg_rd_limit_2, fifo_reqs_2, fifo_rsps_2);
   
   // ----------------

   method Action start (BC_AEId aeid, BC_MC chan,                    // for $displays only
			BC_Addr rd_base_1,  BC_Addr rd_limit_1,
			BC_Addr rd_base_2,  BC_Addr rd_limit_2,
			BC_Addr wr_base,    BC_Addr wr_limit) if (! rg_running);
      rg_aeid        <= aeid;

      rg_rd_base_1   <= rd_base_1;
      rg_rd_limit_1  <= rd_limit_1;
      rg_rd_active_1 <= (rd_base_1 < rd_limit_1);

      rg_rd_base_2   <= rd_base_2;
      rg_rd_limit_2  <= rd_limit_2;
      rg_rd_active_2 <= (rd_base_2 < rd_limit_2);

      rg_wr_base     <= wr_base;
      rg_wr_limit    <= wr_limit;
      rg_wr_active   <= (wr_base < wr_limit);
      rg_wr_rsp_1_drain_count <= 0;
      rg_wr_rsp_2_drain_count <= 0;

      rg_partial_sum <= 0;
      rg_running     <= True;
   endmethod

   method ActionValue #(BC_Data) result () if (rg_running
					       && (! rg_wr_active)
					       && (rg_wr_rsp_1_drain_count == 0)
					       && (rg_wr_rsp_2_drain_count == 0));
      rg_running <= False;
      return rg_partial_sum;
   endmethod

   interface mc_client_pair = tuple2 (ifc_e, ifc_o);
endmodule

// ================================================================

endpackage
