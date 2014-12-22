// Copyright (c) 2012-2013 Bluespec, Inc.  All Rights Reserved.
//
// Author: Rishiyur S. Nikhil

package App_HW;

// ================================================================
// This is Version 1 of the Vector-Add example for the Bluespec-Convey
// library (BClib).  Here, we assume that all three vectors are
// identically aligned w.r.t. the MCs.  Hence, memory requests and
// responses go to statically specified MCs.

// This version assumes out-of-order memory read-responses, i.e., the Convey PDK
// Verilog should be given the flag MC_READ_ORDER=0

// ================================================================
// BSV Library imports

import Vector           :: *;
import FIFOF            :: *;
import GetPut           :: *;
import ClientServer     :: *;
import Connectable      :: *;
import StmtFSM          :: *;
import FShow            :: *;

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

   match { .v8_mc_client_pairs, .v8_mc_server_pairs } <- mkBC_Vec_MC_Port_Ordering_Transactors;

   // ----------------------------------------------------------------
   // Instantiate the App and connect to mem transactors
   
   BC_HW2_IFC hw2 <- mkApp_HW2;
   mkConnection (hw2.mc_ifcs, v8_mc_server_pairs);

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
	    delay (100);    // to allow final writes to reach memory.
	    simple_dispatch_app_ifc.put_caep_result.put (0); // signal caep inst completion
	 endseq
      endseq
      );

   // ================================================================
   // INTERFACE

   interface mc_ifcs        = v8_mc_client_pairs;
   interface dispatch_ifc   = top_dispatch_ifc;
   interface management_ifc = top_management_ifc;
endmodule: mkApp_HW

// ================================================================
// HW module with simplified API

// ----------------

interface BC_HW2_IFC;
   method Action start (BC_AEId fpga_id, BC_Data param_block_addr);
   method Action waitTillDone;

   interface Vector #(8, BC_MC_Client_Pair) mc_ifcs;
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
   // Memory FIFOs and ports

   Vector #(8, FIFOF #(BC_MC_rd_req))    f_rd_reqs_e    <- replicateM (mkFIFOF);
   Vector #(8, FIFOF #(BC_MC_wr_req))    f_wr_reqs_e    <- replicateM (mkFIFOF);
   Vector #(8, FIFOF #(BC_MC_rd_rsp))    f_rd_rsps_e    <- replicateM (mkFIFOF);
   Vector #(8, FIFOF #(BC_MC_flush_req)) f_flush_reqs_e <- replicateM (mkFIFOF);
   Vector #(8, FIFOF #(BC_MC_flush_rsp)) f_flush_rsps_e <- replicateM (mkFIFOF);

   Vector #(8, FIFOF #(BC_MC_rd_req))    f_rd_reqs_o    <- replicateM (mkFIFOF);
   Vector #(8, FIFOF #(BC_MC_wr_req))    f_wr_reqs_o    <- replicateM (mkFIFOF);
   Vector #(8, FIFOF #(BC_MC_rd_rsp))    f_rd_rsps_o    <- replicateM (mkFIFOF);
   Vector #(8, FIFOF #(BC_MC_flush_req)) f_flush_reqs_o <- replicateM (mkFIFOF);
   Vector #(8, FIFOF #(BC_MC_flush_rsp)) f_flush_rsps_o <- replicateM (mkFIFOF);

   function Action send_rd_req (BC_Addr base, Integer param_id);
      action
	 let addr = base + fromInteger (param_id * 64);
	 let req = BC_MC_rd_req {size: BC_8B, addr: addr, rdctl: 0};
	 f_rd_reqs_e [param_id].enq (req);
      endaction
   endfunction

   function ActionValue #(BC_Addr) recv_rd_rsp (Integer param_id);
      actionvalue
	 let rsp = f_rd_rsps_e [param_id].first;
	 f_rd_rsps_e [param_id].deq;
	 return truncate (rsp.data);
      endactionvalue
   endfunction

   function Action send_wr_req (BC_Addr base, Integer param_id, BC_Data x);
      action
	 let addr = base + fromInteger (param_id * 64);
	 let req = BC_MC_wr_req {size: BC_8B, addr: addr, data: x};
	 f_wr_reqs_e [param_id].enq (req);
      endaction
   endfunction

   // ----------------------------------------------------------------
   // Instantiate 8 vadders, and connect them to the MC ports

   Vector #(8, ChanVadd_IFC) v_chanvadds <- replicateM (mkChanVadd);

   for (Integer mc = 0; mc < 8; mc = mc + 1) begin
      let mc_server_pair_mc = tuple2 (fn_FIFOFs_to_MC_Server (f_rd_reqs_e [mc],
							      f_wr_reqs_e [mc],
							      f_rd_rsps_e [mc],
							      f_flush_reqs_e [mc],
							      f_flush_rsps_e [mc]),
				      fn_FIFOFs_to_MC_Server (f_rd_reqs_o [mc],
							      f_wr_reqs_o [mc],
							      f_rd_rsps_o [mc],
							      f_flush_reqs_o [mc],
							      f_flush_rsps_o [mc]));
      mkConnection (v_chanvadds[mc].mc_client_pair, mc_server_pair_mc);
   end

   // ----------------------------------------------------------------
   // MAIN BEHAVIOR 

   Reg #(UInt #(4)) rg_mc <- mkRegU;

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
	 action
	    let vsize   <- recv_rd_rsp (param_VSIZE);
	    let vin1_p  <- recv_rd_rsp (param_VIN1_P);
	    let vin2_p  <- recv_rd_rsp (param_VIN2_P);
	    let vout_p  <- recv_rd_rsp (param_VOUT_P);
	    $display ("%0d: mkApp_HW2 [%0d]: params are %0d 0x%0h 0x%0h 0x%0h; starting per MC vadds",
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
	 // Write final (per-FPGA) sum back to param block
	 send_wr_req (rg_param_block_addr, param_PARTIAL_SUM, rg_partial_sum);
      endseq
      );

   // ----------------------------------------------------------------
   // INTERFACE

   function BC_MC_Client_Pair  fn_mkMC_Client_Pair (Integer mc);
      return tuple2 (fn_FIFOFs_to_MC_Client (f_rd_reqs_e [mc],
					     f_wr_reqs_e [mc],
					     f_rd_rsps_e [mc],
					     f_flush_reqs_e [mc],
					     f_flush_rsps_e [mc]),
		     fn_FIFOFs_to_MC_Client (f_rd_reqs_o [mc],
					     f_wr_reqs_o [mc],
					     f_rd_rsps_o [mc],
					     f_flush_reqs_o [mc],
					     f_flush_rsps_o [mc]));
   endfunction

   method Action start (BC_AEId fpga_id, BC_Data param_block_addr) if (fsm.done);
      rg_fpga_id          <= fpga_id;
      rg_param_block_addr <= truncate (param_block_addr) + (extend (fpga_id) << 3);
      fsm.start;
   endmethod

   method Action waitTillDone() if (fsm.done);
      noAction;
   endmethod

   interface mc_ifcs = genWith (fn_mkMC_Client_Pair);
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

   interface BC_MC_Client_Pair  mc_client_pair;
endinterface

(* synthesize *)
module mkChanVadd (ChanVadd_IFC);
   // These are only needed for $displays
   Reg #(BC_AEId)  rg_aeid <- mkRegU;
   Reg #(BC_MC)    rg_chan <- mkRegU;

   Reg #(BC_Data)         rg_partial_sum <- mkRegU;

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

   // Mem req/rsp fifos
   FIFOF #(BC_Data)       fifo_rd_rsps_e <- mkFIFOF;
   FIFOF #(BC_MC_wr_req)  fifo_wr_reqs_e <- mkFIFOF;

   FIFOF #(BC_Data)       fifo_rd_rsps_o <- mkFIFOF;
   FIFOF #(BC_MC_wr_req)  fifo_wr_reqs_o <- mkFIFOF;

   // ----------------------------------------------------------------
   // Receive a 64b word from each of vin1 and vin2, write sum to vout,
   // and accumulate into partial sum

   rule rl_sum_and_gen_wr_reqs (rg_wr_active);
      let x1 = fifo_rd_rsps_e.first;  fifo_rd_rsps_e.deq;    // from vin1
      let x2 = fifo_rd_rsps_o.first;  fifo_rd_rsps_o.deq;    // from vin2
      let x3 = x1 + x2;

      // Write x3 to vout. Alternate between even and odd ports.
      let fifo_wr_reqs = ((rg_wr_base [3] == 1'b0) ? fifo_wr_reqs_e : fifo_wr_reqs_o );
      let wr_req       = BC_MC_wr_req { size:BC_8B, addr: rg_wr_base, data: x3 };
      fifo_wr_reqs.enq (wr_req);

      // Accumulate x3 in partial sum
      rg_partial_sum <= rg_partial_sum + x3;
      let next_base = bc_next_addr_8B_in_chan (rg_wr_base);
      rg_wr_base <= next_base;
      rg_wr_active <= (next_base < rg_wr_limit);
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   function BC_MC_Client  mkMC_Client (Reg #(Bool)            rg_rd_active,
				       Reg #(BC_Addr)         rg_rd_base,
				       Reg #(BC_Addr)         rg_rd_limit,
				       FIFOF #(BC_MC_wr_req)  fifo_wr_reqs,
				       FIFOF #(BC_Data)       fifo_rd_rsps);
      return interface BC_MC_Client;
		interface Get get_rd_req;
		   // Reads from an input vector
		   method ActionValue #(BC_MC_rd_req) get () if (rg_rd_active);
		      let req  = BC_MC_rd_req { size:BC_8B, addr: rg_rd_base, rdctl:?};

		      let next_base = bc_next_addr_8B_in_chan (rg_rd_base);
		      rg_rd_base   <= next_base;
		      rg_rd_active <= (next_base < rg_rd_limit);
		      return req;
		   endmethod
		endinterface

		interface Get get_wr_req = toGet (fifo_wr_reqs);

		interface Put put_rd_rsp;
		   method Action put (BC_MC_rd_rsp rsp) if (rg_running);
		      fifo_rd_rsps.enq (rsp.data);
		   endmethod
		endinterface

		interface Get get_flush_req = getstub;
		interface Put put_flush_rsp = putstub;
	     endinterface;
   endfunction

   let ifc_e = mkMC_Client (rg_rd_active_1, rg_rd_base_1, rg_rd_limit_1, fifo_wr_reqs_e, fifo_rd_rsps_e);

   let ifc_o = mkMC_Client (rg_rd_active_2, rg_rd_base_2, rg_rd_limit_2, fifo_wr_reqs_o, fifo_rd_rsps_o);
   
   // ----------------

   method Action start (BC_AEId aeid, BC_MC chan,                    // for $displays only
			BC_Addr rd_base_1,  BC_Addr rd_limit_1,
			BC_Addr rd_base_2,  BC_Addr rd_limit_2,
			BC_Addr wr_base,    BC_Addr wr_limit) if (! rg_running);
      rg_aeid        <= aeid;
      rg_chan        <= chan;

      rg_rd_base_1   <= rd_base_1;
      rg_rd_limit_1  <= rd_limit_1;
      rg_rd_active_1 <= (rd_base_1 < rd_limit_1);

      rg_rd_base_2   <= rd_base_2;
      rg_rd_limit_2  <= rd_limit_2;
      rg_rd_active_2 <= (rd_base_2 < rd_limit_2);

      rg_wr_base     <= wr_base;
      rg_wr_limit    <= wr_limit;
      rg_wr_active   <= (wr_base < wr_limit);

      rg_partial_sum <= 0;
      rg_running     <= True;
   endmethod

   method ActionValue #(BC_Data) result () if (rg_running && (! rg_wr_active));
      rg_running <= False;
      return rg_partial_sum;
   endmethod

   interface mc_client_pair = tuple2 (ifc_e, ifc_o);
endmodule

// ================================================================

endpackage
