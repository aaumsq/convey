// Copyright (c) 2012-2013 Bluespec, Inc.  All Rights Reserved.
//
// Author: Rishiyur S. Nikhil

package App_HW;

// ================================================================
// This is Version 2 of the Vector-Add example for the Bluespec-Convey
// library (BClib). Here, we make no assumptions about the alignments
// of the tree vectors w.r.t. MCs.  Hence, we use a Memory Network to
// route memory requests and responses properly.

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
import BC_Mem_Tree        :: *;
import CompletionBuffer2  :: *;

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
   // Instantiate the App and use its mem ports directly
   
   BC_HW2_IFC hw2 <- mkApp_HW2;

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

   interface mc_ifcs        = hw2.mc_ifcs;
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
Integer param_STATUS      = 5;

// ----------------
// # of Vadders
typedef  8  M;    // # of VAdders

// The following are for sizing the mem req/rsp routing trees
// The root of the routing tree has 2M+1 leafside ports
//   2M are used for the Vadders for reading/writing the vectors, and
//   1 is used by mkApp_HW2 for reading the param block
typedef  TAdd #(M,M)   MM;

typedef  TAdd #(M,1)   M1;
typedef  TAdd #(MM,1)  MM1;

Integer  iM  = valueOf (M);
Integer  iMM = valueOf (MM);

// ----------------

(* synthesize *)
module mkApp_HW2 (BC_HW2_IFC);
   Reg #(BC_AEId)         rg_fpga_id          <- mkRegU;
   Reg #(BC_Addr)         rg_param_block_addr <- mkRegU;
   Reg #(BC_Data)         rg_partial_sum      <- mkRegU;

   Reg #(BC_Addr)         rg_vsize            <- mkRegU;
   Reg #(BC_Addr)         rg_vin1_p           <- mkRegU;
   Reg #(BC_Addr)         rg_vin2_p           <- mkRegU;
   Reg #(BC_Addr)         rg_vout_p           <- mkRegU;

   // ----------------------------------------------------------------
   // Memory routing networks

   MemRdTreeNode #(M, 8)   rd_tree_e <- mkRdTreeRoot_M_8;
   MemRdTreeNode #(M1, 8)  rd_tree_o <- mkRdTreeRoot_M1_8;

   MemWrTreeNode #(M, 8)   wr_tree_e <- mkWrTreeRoot_M_8;
   MemWrTreeNode #(M1, 8)  wr_tree_o <- mkWrTreeRoot_M1_8;

   FIFOF #(BC_Mem_rd_rsp)  f_rd_rsps <- mkFIFOF;
   mkConnection (rd_tree_o.leafside [fromInteger (iM)].response, toPut (f_rd_rsps));

   function Action send_rd_req (BC_Addr base, Integer param_id);
      action
	 let addr  = base + fromInteger (param_id * 64);
	 let rdctl = fromInteger (param_id);
	 let req   = tuple2 (addr, rdctl);
	 rd_tree_o.leafside [fromInteger (iM)].request.put (req);
      endaction
   endfunction

   function Action recv_rd_rsp (Reg #(BC_Addr) rg, Integer param_id);
      action
	 match { .d, .rdctl } = f_rd_rsps.first;
	 await (rdctl == fromInteger (param_id));
	 f_rd_rsps.deq;
	 rg <= truncate (d);
      endaction
   endfunction

   function Action send_wr_req (BC_Addr base, Integer param_id, BC_Data x);
      action
	 let addr = base + fromInteger (param_id * 64);
	 let req = tuple2 (addr, x);
	 wr_tree_o.leafside [fromInteger (iM)].put (req);
      endaction
   endfunction

   // ----------------------------------------------------------------
   // Instantiate M vadders, and connect them to the mem tree

   Vector #(M, Vadder_IFC) v_vadders <- replicateM (mkVadder);

   for (Integer m = 0; m < iM; m = m + 1) begin
      mkConnection (rd_tree_e.leafside [m], v_vadders [m].rd_client_e);
      mkConnection (rd_tree_o.leafside [m], v_vadders [m].rd_client_o);

      mkConnection (wr_tree_e.leafside [m], v_vadders [m].wr_client_e);
      mkConnection (wr_tree_o.leafside [m], v_vadders [m].wr_client_o);
   end

   // ----------------------------------------------------------------
   // MAIN BEHAVIOR 

   Reg #(UInt #(8)) rg_m       <- mkRegU;
   Reg #(BC_Addr)   rg_vsize_j <- mkRegU;
   Reg #(BC_Addr)   rg_offset  <- mkRegU;

   let fsm <- mkFSM (
      seq
	 // Send read requests for the parameters for this FPGA (sequentially)
	 send_rd_req (rg_param_block_addr, param_VSIZE);
	 send_rd_req (rg_param_block_addr, param_VIN1_P);
	 send_rd_req (rg_param_block_addr, param_VIN2_P);
	 send_rd_req (rg_param_block_addr, param_VOUT_P);

	 // Receive the parameters for this FPGA (read responses, in any order)
	 par
	    recv_rd_rsp (rg_vsize,  param_VSIZE);
	    recv_rd_rsp (rg_vin1_p, param_VIN1_P);
	    recv_rd_rsp (rg_vin2_p, param_VIN2_P);
	    recv_rd_rsp (rg_vout_p, param_VOUT_P);
	 endpar
	 action
	    $write ("%0d: mkApp_HW2 [%0d]: params are %0d 0x%0h 0x%0h 0x%0h; starting vadders\n",
		    cur_cycle, rg_fpga_id, rg_vsize, rg_vin1_p, rg_vin2_p, rg_vout_p);
	    rg_vsize_j <= (rg_vsize / fromInteger (iM)) << 3;
	    rg_offset  <= 0;
	 endaction

	 // For all vadders but the last, the vector size is vsize_j
	 for (rg_m <= 0; rg_m < fromInteger (iM-1); rg_m <= rg_m + 1) action
	       v_vadders [rg_m].start (rg_fpga_id, truncate (pack (rg_m)),
				       rg_param_block_addr + fromInteger (param_STATUS * 64),
				       rg_vin1_p + rg_offset, rg_vin1_p + rg_offset + rg_vsize_j,
				       rg_vin2_p + rg_offset, rg_vin2_p + rg_offset + rg_vsize_j,
				       rg_vout_p + rg_offset, rg_vout_p + rg_offset + rg_vsize_j);
	       rg_offset <= rg_offset + rg_vsize_j;
	 endaction

	 // For the last vadder, the vector size is all the remaining items
	 v_vadders [iM - 1].start (rg_fpga_id, fromInteger (iM - 1),
				   rg_param_block_addr + fromInteger (param_STATUS * 64),
				   rg_vin1_p + rg_offset, rg_vin1_p + (rg_vsize << 3),
				   rg_vin2_p + rg_offset, rg_vin2_p + (rg_vsize << 3),
				   rg_vout_p + rg_offset, rg_vout_p + (rg_vsize << 3));

	 // Collect per-vadder partial sums, accumulate into full (per-FPGA) sum
	 rg_partial_sum <= 0;
	 for (rg_m <= 0; rg_m < fromInteger (iM); rg_m <= rg_m + 1) action
	    let vadder_sum <- v_vadders [rg_m].result;
	    rg_partial_sum <= rg_partial_sum + vadder_sum;
	 endaction
	 // Write final (per-FPGA) sum back to param block
	 send_wr_req (rg_param_block_addr, param_PARTIAL_SUM, rg_partial_sum);
      endseq
      );

   // ----------------------------------------------------------------
   // INTERFACE

   Vector #(8, BC_MC_Client_Pair) v_ifcs;
   for (Integer mc = 0; mc < 8; mc = mc + 1) begin
      v_ifcs [mc] = tuple2 (interface BC_MC_Client;
			       interface get_rd_req = to_get_rd_req (rd_tree_e.rootside [mc].request);
			       interface put_rd_rsp = to_put_rd_rsp (rd_tree_e.rootside [mc].response);
			       interface get_wr_req = to_get_wr_req (wr_tree_e.rootside [mc]);
			       interface get_flush_req = getstub;
			       interface put_flush_rsp = putstub;
			    endinterface,
			    interface BC_MC_Client;
			       interface get_rd_req = to_get_rd_req (rd_tree_o.rootside [mc].request);
			       interface put_rd_rsp = to_put_rd_rsp (rd_tree_o.rootside [mc].response);
			       interface get_wr_req = to_get_wr_req (wr_tree_o.rootside [mc]);
			       interface get_flush_req = getstub;
			       interface put_flush_rsp = putstub;
			    endinterface);
   end

   method Action start (BC_AEId fpga_id, BC_Data param_block_addr) if (fsm.done);
      rg_fpga_id          <= fpga_id;
      rg_param_block_addr <= truncate (param_block_addr) + (extend (fpga_id) << 3);
      rd_tree_e.init (fpga_id);
      rd_tree_o.init (fpga_id);
      wr_tree_e.init (fpga_id);
      wr_tree_o.init (fpga_id);
      fsm.start;
   endmethod

   method Action waitTillDone() if (fsm.done);
      noAction;
   endmethod

   interface mc_ifcs = v_ifcs;
endmodule

// ================================================================
// Separately synthesized instances of the mem tree roots

(* synthesize *)
module mkRdTreeRoot_M_8 (MemRdTreeNode #(M, 8));
   Bool isRoot = True;
   let ifc <- mkMemRdTreeNode (isRoot);
   return ifc;
endmodule

(* synthesize *)
module mkRdTreeRoot_M1_8 (MemRdTreeNode #(M1, 8));
   Bool isRoot = True;
   let ifc <- mkMemRdTreeNode (isRoot);
   return ifc;
endmodule

(* synthesize *)
module mkWrTreeRoot_M_8 (MemWrTreeNode #(M, 8));
   Bool isRoot = True;
   let ifc <- mkMemWrTreeNode (isRoot);
   return ifc;
endmodule

(* synthesize *)
module mkWrTreeRoot_M1_8 (MemWrTreeNode #(M1, 8));
   Bool isRoot = True;
   let ifc <- mkMemWrTreeNode (isRoot);
   return ifc;
endmodule

// ================================================================
// The following module is instantiated M times
// It performs the vector-add for a VSIZE/M chunk of the vectors

interface Vadder_IFC;
   method Action start (BC_AEId aeid, BC_MC chan, BC_Addr status_p,
			BC_Addr rd_base_1,  BC_Addr rd_limit_1,
			BC_Addr rd_base_2,  BC_Addr rd_limit_2,
			BC_Addr wr_base,    BC_Addr wr_limit);
   method ActionValue #(BC_Data) result ();    // partial sum

   // Memory interfaces
   interface BC_Mem_rd_client    rd_client_e;
   interface BC_Mem_rd_client    rd_client_o;
   interface BC_Mem_wr_client    wr_client_e;
   interface BC_Mem_wr_client    wr_client_o;
endinterface

(* synthesize *)
module mkVadder (Vadder_IFC);
   Reg #(Bool) rg_initialize <- mkReg (False);
   Reg #(Bool) rg_rd_active  <- mkReg (False);
   Reg #(Bool) rg_wr_active  <- mkReg (False);

   // These are only needed for $displays
   Reg #(BC_AEId)  rg_aeid     <- mkRegU;
   Reg #(BC_MC)    rg_chan     <- mkRegU;
   Reg #(BC_Addr)  rg_status_p <- mkRegU;

   Reg #(BC_Data)         rg_partial_sum <- mkRegU;

   // Vector 1 (input) params
   Reg #(BC_Addr)         rg_rd_base_e   <- mkRegU;
   Reg #(BC_Addr)         rg_rd_limit_e  <- mkRegU;
   Reg #(Bit #(20))       rg_jr_e_prev <- mkRegU;
   Reg #(Bit #(20))       rg_jr_e      <- mkRegU;
   FIFOF #(BC_Mem_rd_req) f_rd_reqs_e <- mkFIFOF;
   CompletionBuffer2 #(100, BC_Data) cb_rsps_e <- mkCompletionBuffer2;
   FIFOF #(BC_Mem_wr_req)  fifo_wr_reqs_e <- mkFIFOF;

   // Vector 2 (input) params
   Reg #(BC_Addr)         rg_rd_base_o   <- mkRegU;
   Reg #(BC_Addr)         rg_rd_limit_o  <- mkRegU;
   Reg #(Bit #(20))       rg_jr_o_prev <- mkRegU;
   Reg #(Bit #(20))       rg_jr_o      <- mkRegU;
   FIFOF #(BC_Mem_rd_req) f_rd_reqs_o <- mkFIFOF;
   CompletionBuffer2 #(100, BC_Data) cb_rsps_o <- mkCompletionBuffer2;
   FIFOF #(BC_Mem_wr_req)  fifo_wr_reqs_o <- mkFIFOF;

   // Vector 3 (output) params
   Reg #(BC_Addr)         rg_wr_base     <- mkRegU;
   Reg #(BC_Addr)         rg_wr_limit    <- mkRegU;
   Reg #(Bit #(20))       rg_jw_prev   <- mkRegU;
   Reg #(Bit #(20))       rg_jw        <- mkRegU;

   // ----------------------------------------------------------------
   // Initialize

   rule rl_initialize (rg_initialize);
      rg_rd_active   <= (rg_rd_base_e < rg_rd_limit_e);    // should be same with _o values
      rg_wr_active   <= (rg_wr_base   < rg_wr_limit);

      rg_partial_sum <= 0;

      rg_jr_e      <= 0;      rg_jr_e_prev <= 0;
      rg_jr_o      <= 0;      rg_jr_o_prev <= 0;
      rg_jw        <= 0;      rg_jw_prev   <= 0;
      rg_initialize <= False;
   endrule

   // ----------------------------------------------------------------
   // Generate read addresses
   // and accumulate into partial sum

   function ActionValue #(BC_Mem_rd_req)
       fn_gen_rd_req (Reg #(BC_Addr) rg_rd_base,
		      Reg #(BC_Addr) rg_rd_limit,
		      CompletionBuffer2 #(100, BC_Data) cb_rsps,
		      Reg #(Bit #(20)) rg_jr);
      actionvalue
	 let cbtoken <- cb_rsps.reserve.get;
	 let req  = tuple2 (rg_rd_base, extend (pack (cbtoken)));
	 let next_base = rg_rd_base + 8;
	 rg_rd_base   <= next_base;
	 rg_jr        <= rg_jr + 1;
	 return req;
      endactionvalue
   endfunction

   rule rl_gen_rd_addrs (rg_rd_active);
      let rd_req_e <- fn_gen_rd_req (rg_rd_base_e, rg_rd_limit_e, cb_rsps_e, rg_jr_e);
      f_rd_reqs_e.enq (rd_req_e);

      let rd_req_o <- fn_gen_rd_req (rg_rd_base_o, rg_rd_limit_o, cb_rsps_o, rg_jr_o);
      f_rd_reqs_o.enq (rd_req_o);

      rg_rd_active <= (rg_rd_base_e + 8 < rg_rd_limit_e);    // should be same with _o values
   endrule

   // ----------------------------------------------------------------
   // Receive a 64b word from each of vin1 and vin2, write sum to vout,
   // and accumulate into partial sum

   rule rl_sum_and_gen_wr_reqs (rg_wr_active);
      let x1 <- cb_rsps_e.drain.get;    // from vin1
      let x2 <- cb_rsps_o.drain.get;    // from vin2
      let x3 = x1 + x2;

      // Write x3 to vout. Alternate between even and odd ports.
      let wr_req       = tuple2 (rg_wr_base, x3);
      let fifo_wr_reqs = ((rg_wr_base [3] == 1'b0) ? fifo_wr_reqs_e : fifo_wr_reqs_o );
      fifo_wr_reqs.enq (wr_req);

      // Accumulate x3 in partial sum
      rg_partial_sum <= rg_partial_sum + x3;
      let next_base = rg_wr_base + 8;
      rg_wr_base <= next_base;
      rg_wr_active <= (next_base < rg_wr_limit);
      rg_jw <= rg_jw + 1;
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   function BC_Mem_rd_client mkClient (FIFOF #(BC_Mem_rd_req) f_rd_reqs,
				       CompletionBuffer2 #(100, BC_Data) cb_rsps);
      return interface Client;
		interface Get request = toGet (f_rd_reqs);
		interface Put response;
		   method Action put (BC_Mem_rd_rsp  rsp);
		      match { .data, .rdctl } = rsp;
		      let tag = unpack (truncate (rdctl));
		      cb_rsps.complete.put (tuple2 (tag, data));
		   endmethod
		endinterface
	     endinterface;
   endfunction

   method Action start (BC_AEId aeid, BC_MC chan, BC_Addr status_p,
			BC_Addr rd_base_e,  BC_Addr rd_limit_e,
			BC_Addr rd_base_o,  BC_Addr rd_limit_o,
			BC_Addr wr_base,    BC_Addr wr_limit);
      rg_aeid        <= aeid;
      rg_chan        <= chan;
      rg_status_p    <= status_p;

      rg_rd_base_e   <= rd_base_e;      rg_rd_limit_e  <= rd_limit_e;
      rg_rd_base_o   <= rd_base_o;      rg_rd_limit_o  <= rd_limit_o;
      rg_wr_base     <= wr_base;        rg_wr_limit    <= wr_limit;

      rg_initialize  <= True;
   endmethod

   method ActionValue #(BC_Data) result () if (! rg_wr_active);
      return rg_partial_sum;
   endmethod

   interface rd_client_e = mkClient (f_rd_reqs_e, cb_rsps_e);
   interface wr_client_e = toGet (fifo_wr_reqs_e);
   interface rd_client_o = mkClient (f_rd_reqs_o, cb_rsps_o);
   interface wr_client_o = toGet (fifo_wr_reqs_o);
   
endmodule

// ================================================================

endpackage
