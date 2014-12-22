// Copyright (c) 2012-2015 Bluespec, Inc.  All Rights Reserved.
//
// Author: Rishiyur S. Nikhil

package App_HW;

// ================================================================
// This is Version 2 of the Vector-Add example for the Bluespec-Convey
// library (BClib). Here, we make no assumptions about the alignments
// of the tree vectors w.r.t. MCs.  Hence, we use a Memory Network to
// route memory requests and responses properly.  We also make no
// assumptions about memory ordering.
// When compiled with the Convey PDK, therefore, one can use:
//     MC_READ_ORDER = 0    (PDK does not have re-ordering logic)
//     MC_XBAR = 0          (PDK does not have a routing crossbar)

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
	    delay (200);    // to allow final writes to reach memory.
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

   interface Vector #(16, BC_MC_Client) mc_ifcs;
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

   MemTreeNode #(M, 8)   mem_tree_e <- mkMemTreeRoot_M_8;
   MemTreeNode #(M1, 8)  mem_tree_o <- mkMemTreeRoot_M1_8;

   FIFOF #(BC_MC_RSP) f_rsps <- mkFIFOF;
   mkConnection (mem_tree_o.leafside [iM].response, toPut (f_rsps));

   // ----------------------------------------------------------------
   // These functions are for reading and writing parameters
   // They use the last, extra port in mem_tree_o, at index [iM]

   function Action send_rd_req (BC_Addr base, Integer param_id);
      action
	 let addr  = base + fromInteger (param_id * 64);
	 let req = BC_MC_REQ {cmd_sub: REQ_RD, rtnctl: fromInteger (param_id), len: BC_8B,
			      vadr: addr, data: ?};
	 mem_tree_o.leafside [iM].request.put (req);
      endaction
   endfunction

   function Action recv_rd_rsp (Reg #(BC_Addr) rg, Integer param_id);
      action
	 let rsp = f_rsps.first; f_rsps.deq;
	 await (rsp.rtnctl == fromInteger (param_id));
	 rg <= truncate (rsp.data);
      endaction
   endfunction

   function Action send_wr_req (BC_Addr base, Integer param_id, BC_Data x);
      action
	 let addr = base + fromInteger (param_id * 64);
	 let req = BC_MC_REQ {cmd_sub: REQ_WR, rtnctl: fromInteger (param_id), len: BC_8B,
			      vadr: addr, data: x};
	 mem_tree_o.leafside [iM].request.put (req);
      endaction
   endfunction

   // ----------------------------------------------------------------
   // Instantiate M vadders, and connect them to the mem tree

   Vector #(M, Vadder_IFC) v_vadders <- replicateM (mkVadder);

   for (Integer m = 0; m < iM; m = m + 1) begin
      let mem_server_pair = tuple2 (mem_tree_e.leafside [m],
				    mem_tree_o.leafside [m]);
      mkConnection (v_vadders [m].mem_client_pair, mem_server_pair);
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
				       rg_vin1_p + rg_offset, rg_vin1_p + rg_offset + rg_vsize_j,
				       rg_vin2_p + rg_offset, rg_vin2_p + rg_offset + rg_vsize_j,
				       rg_vout_p + rg_offset, rg_vout_p + rg_offset + rg_vsize_j);
	       rg_offset <= rg_offset + rg_vsize_j;
	 endaction

	 // For the last vadder, the vector size is all the remaining items
	 v_vadders [iM - 1].start (rg_fpga_id, fromInteger (iM - 1),
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

   function BC_MC_Client fn_mkMC_Client (Integer mc);
      Vector #(8, BC_Mem_Client) rootside = (((mc % 2) == 0) ? mem_tree_e.rootside : mem_tree_o.rootside);
      return interface BC_MC_Client;
		interface Client req_rsp = rootside [mc / 2];
		interface Client flush;
		   interface Get request  = getstub;
		   interface Put response = putstub;
		endinterface
	     endinterface;
   endfunction

   method Action start (BC_AEId fpga_id, BC_Data param_block_addr) if (fsm.done);
      rg_fpga_id          <= fpga_id;
      rg_param_block_addr <= truncate (param_block_addr) + (extend (fpga_id) << 3);
      mem_tree_e.init (fpga_id);
      mem_tree_o.init (fpga_id);
      fsm.start;
   endmethod

   method Action waitTillDone() if (fsm.done);
      noAction;
   endmethod

   interface mc_ifcs = genWith (fn_mkMC_Client);
endmodule

// ================================================================
// Separately synthesized instances of the mem tree roots

(* synthesize *)
module mkMemTreeRoot_M_8 (MemTreeNode #(M, 8));
   Bool isRoot = True;
   let ifc <- mkMemTreeNode (isRoot);
   return ifc;
endmodule

(* synthesize *)
module mkMemTreeRoot_M1_8 (MemTreeNode #(M1, 8));
   Bool isRoot = True;
   let ifc <- mkMemTreeNode (isRoot);
   return ifc;
endmodule

// ================================================================
// The following module is instantiated M times
// It performs the vector-add for a VSIZE/M chunk of the vectors

interface Vadder_IFC;
   method Action start (BC_AEId aeid, BC_MC chan,
			BC_Addr rd_base_1,  BC_Addr rd_limit_1,
			BC_Addr rd_base_2,  BC_Addr rd_limit_2,
			BC_Addr wr_base,    BC_Addr wr_limit);
   method ActionValue #(BC_Data) result ();    // partial sum

   // Memory interfaces
   interface Tuple2 #(BC_Mem_Client, BC_Mem_Client)  mem_client_pair;
endinterface

(* synthesize *)
module mkVadder (Vadder_IFC);
   // These are only needed for $displays
   Reg #(BC_AEId)  rg_aeid     <- mkRegU;
   Reg #(BC_MC)    rg_chan     <- mkRegU;

   Reg #(Bool) rg_initialize <- mkReg (False);

   Reg #(BC_Data)         rg_partial_sum <- mkRegU;

   // Vector 1 (input) params
   Reg #(BC_Addr)         rg_rd_base_1  <- mkRegU;
   Reg #(BC_Addr)         rg_rd_limit_1 <- mkRegU;
   Reg #(Bool)            rg_rd_active  <- mkReg (False);

   // Vector 2 (input) params
   Reg #(BC_Addr)         rg_rd_base_2  <- mkRegU;
   Reg #(BC_Addr)         rg_rd_limit_2 <- mkRegU;

   // Vector 3 (output) params
   Reg #(BC_Addr)         rg_wr_base   <- mkRegU;
   Reg #(BC_Addr)         rg_wr_limit  <- mkRegU;
   Reg #(Bool)            rg_wr_active <- mkReg (False);

   // Mem req/rsp fifos
   FIFOF #(BC_MC_REQ)                f_reqs_1  <- mkFIFOF;
   CompletionBuffer2 #(100, BC_Data) cb_rsps_1 <- mkCompletionBuffer2;
   FIFOF #(BC_MC_REQ)                f_reqs_2  <- mkFIFOF;
   CompletionBuffer2 #(100, BC_Data) cb_rsps_2 <- mkCompletionBuffer2;

   // ----------------------------------------------------------------
   // Initialize

   rule rl_initialize (rg_initialize);
      rg_rd_active   <= (rg_rd_base_1 < rg_rd_limit_1);    // should be same with _2 values
      rg_wr_active   <= (rg_wr_base   < rg_wr_limit);

      rg_partial_sum <= 0;
      rg_initialize  <= False;
   endrule

   // ----------------------------------------------------------------
   // Generate read addresses
   // and accumulate into partial sum

   function ActionValue #(BC_MC_REQ)
       fn_gen_rd_req (Reg #(BC_Addr) rg_rd_base,
		      Reg #(BC_Addr) rg_rd_limit,
		      CompletionBuffer2 #(100, BC_Data) cb_rsps);
      actionvalue
	 let cbtoken <- cb_rsps.reserve.get;
	 let req  = BC_MC_REQ {cmd_sub: REQ_RD, rtnctl: extend (pack (cbtoken)), len:BC_8B,
			       vadr: rg_rd_base, data:?};
	 let next_base = rg_rd_base + 8;
	 rg_rd_base   <= next_base;
	 return req;
      endactionvalue
   endfunction

   rule rl_gen_rd_addrs (rg_rd_active);
      let rd_req_1 <- fn_gen_rd_req (rg_rd_base_1, rg_rd_limit_1, cb_rsps_1);
      f_reqs_1.enq (rd_req_1);

      let rd_req_2 <- fn_gen_rd_req (rg_rd_base_2, rg_rd_limit_2, cb_rsps_2);
      f_reqs_2.enq (rd_req_2);

      rg_rd_active <= (rg_rd_base_1 + 8 < rg_rd_limit_1);    // should be same with _2 values
   endrule

   // ----------------------------------------------------------------
   // Receive a 64b word from each of vin1 and vin2, write sum to vout,
   // and accumulate into partial sum

   rule rl_sum_and_gen_wr_reqs (rg_wr_active);
      let x1 <- cb_rsps_1.drain.get;    // from vin1
      let x2 <- cb_rsps_2.drain.get;    // from vin2
      let x3 = x1 + x2;

      // Write x3 to vout. Alternate between even and odd ports.
      let wr_req       = BC_MC_REQ {cmd_sub: REQ_WR, rtnctl: 0, len:BC_8B, vadr: rg_wr_base, data: x3};
      let f_reqs = ((rg_wr_base [3] == 1'b0) ? f_reqs_1 : f_reqs_2 );
      f_reqs.enq (wr_req);

      // Accumulate x3 in partial sum
      rg_partial_sum <= rg_partial_sum + x3;
      let next_base = rg_wr_base + 8;
      rg_wr_base <= next_base;
      rg_wr_active <= (next_base < rg_wr_limit);
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   function BC_Mem_Client mkMem_Client (FIFOF #(BC_MC_REQ) f_reqs,
					CompletionBuffer2 #(100, BC_Data) cb_rsps);
      return interface Client;
		interface Get request = toGet (f_reqs);
		interface Put response;
		   method Action put (BC_MC_RSP  rsp);
		      case (rsp.cmd)
			 RSP_RD_DATA: begin
					 let tag = unpack (truncate (rsp.rtnctl));
					 cb_rsps.complete.put (tuple2 (tag, rsp.data));
				      end
			 RSP_WR_CMP : noAction; // just discard write-responses
			 default: begin
				     $display ("INTERNAL ERROR: mem response is not RD_DATA or WR_CMP: ", fshow (rsp));
				     $finish (1);
				  end
		      endcase
		   endmethod
		endinterface
	     endinterface;
   endfunction

   let ifc_e = mkMem_Client (f_reqs_1, cb_rsps_1);

   let ifc_o = mkMem_Client (f_reqs_2, cb_rsps_2);
   
   // ----------------

   method Action start (BC_AEId aeid, BC_MC chan,
			BC_Addr rd_base_1,  BC_Addr rd_limit_1,
			BC_Addr rd_base_2,  BC_Addr rd_limit_2,
			BC_Addr wr_base,    BC_Addr wr_limit);
      rg_aeid        <= aeid;
      rg_chan        <= chan;

      rg_rd_base_1   <= rd_base_1;      rg_rd_limit_1  <= rd_limit_1;
      rg_rd_base_2   <= rd_base_2;      rg_rd_limit_2  <= rd_limit_2;
      rg_wr_base     <= wr_base;        rg_wr_limit    <= wr_limit;

      rg_initialize  <= True;
   endmethod

   method ActionValue #(BC_Data) result () if (! rg_wr_active);
      return rg_partial_sum;
   endmethod

   interface mem_client_pair = tuple2 (ifc_e, ifc_o);
endmodule

// ================================================================

endpackage
