// Copyright (c) 2011-2015 Bluespec, Inc.  All Rights Reserved.
// Distributed under license.

// Author: Rishiyur S. Nikhil

// This package defines modules that can optionally be used in a
// Convey BSV app to perform some standard functions and provide
// simplified interfaces to the app

package BC_Transactors;

// ================================================================
// Library imports

import RegFile           :: *;
import Vector            :: *;
import FIFOF             :: *;
import GetPut            :: *;
import Connectable       :: *;
import CompletionBuffer2 :: *;
import FShow             :: *;

// ----------------
// Local imports

import BC_HW_IFC :: *;

// ****************************************************************
// ****************************************************************
// ****************************************************************
// MC (Memory Controller) Transactors

// ================================================================
// Null transactor just passes reqs and responses through
// So, response ordering remains unchanged (i.e., unordered)

(* synthesize *)
module mkBC_MC_Port_Null_Transactor (Tuple2 #(BC_MC_Client, BC_MC_Server));

   FIFOF #(BC_MC_rd_req)     fifo_rd_reqs    <- mkFIFOF;
   FIFOF #(BC_MC_wr_req)     fifo_wr_reqs    <- mkFIFOF;
   FIFOF #(BC_MC_rd_rsp)     fifo_rd_rsps    <- mkFIFOF;
   FIFOF #(BC_MC_flush_req)  fifo_flush_reqs <- mkFIFOF;
   FIFOF #(BC_MC_flush_rsp)  fifo_flush_rsps <- mkFIFOF;

   let c_ifc = interface BC_MC_Client;
		  interface Get  get_rd_req    = toGet (fifo_rd_reqs);
		  interface Get  get_wr_req    = toGet (fifo_wr_reqs);
		  interface Put  put_rd_rsp    = toPut (fifo_rd_rsps);
		  interface Get  get_flush_req = toGet (fifo_flush_reqs);
		  interface Put  put_flush_rsp = toPut (fifo_flush_rsps);
	       endinterface;

   let s_ifc = interface BC_MC_Server;
		  interface Put  put_rd_req    = toPut (fifo_rd_reqs);
		  interface Put  put_wr_req    = toPut (fifo_wr_reqs);
		  interface Get  get_rd_rsp    = toGet (fifo_rd_rsps);
		  interface Put  put_flush_req = toPut (fifo_flush_reqs);
		  interface Get  get_flush_rsp = toGet (fifo_flush_rsps);
	       endinterface;

   return tuple2 (c_ifc, s_ifc);
endmodule: mkBC_MC_Port_Null_Transactor

// ----------------------------------------------------------------
// Vector of MC port null transactors: 8 MCs x even/odd

(* synthesize *)
module mkBC_Vec_MC_Port_Null_Transactors
   (Tuple2 #(Vector #(8, BC_MC_Client_Pair),
	     Vector #(8, BC_MC_Server_Pair)));

   Vector #(8, Tuple2 #(BC_MC_Client,
			BC_MC_Server))
       evens <- replicateM (mkBC_MC_Port_Null_Transactor);

   Vector #(8, Tuple2 #(BC_MC_Client,
			BC_MC_Server))
       odds <- replicateM (mkBC_MC_Port_Null_Transactor);

   return tuple2 (zip (map (tpl_1, evens), map (tpl_1, odds)),
		  zip (map (tpl_2, evens), map (tpl_2, odds)));
endmodule: mkBC_Vec_MC_Port_Null_Transactors

// ================================================================
// Ordering transactor internally re-orders responses so that they
// are returned to the app in request order.
// Note: re-order tags are shifted in to the LSBs of the rdctl tag
// (thereby losing the MSBs of rdctl)
// CompletionBuffer below is sized at 100 because Convey mem latency is ~100

(* synthesize *)
module mkBC_MC_Port_Ordering_Transactor (Tuple2 #(BC_MC_Client, BC_MC_Server));

   FIFOF #(BC_MC_rd_req)                   fifo_rd_reqs    <- mkFIFOF;
   FIFOF #(BC_MC_wr_req)                   fifo_wr_reqs    <- mkFIFOF;
   // WARNING: buffer size and tokenBitSize, below, are related
   CompletionBuffer2 #(100, BC_MC_rd_rsp)  cb              <- mkCompletionBuffer2;
   FIFOF #(BC_MC_flush_req)                fifo_flush_reqs <- mkFIFOF;
   FIFOF #(BC_MC_flush_rsp)                fifo_flush_rsps <- mkFIFOF;

   // WARNING: should be ceiling (log_2 (CompletionBuffer2 size))
   Integer tokenBitSize = 7;

   let c_ifc = interface BC_MC_Client;
		  interface Get  get_rd_req = toGet (fifo_rd_reqs);
		  interface Get  get_wr_req = toGet (fifo_wr_reqs);
		  interface Put  put_rd_rsp;
		     method Action put (BC_MC_rd_rsp rsp);
			let tok = unpack (truncate (rsp.rdctl));
			let rsp2 = BC_MC_rd_rsp { data:rsp.data, rdctl: (rsp.rdctl >> tokenBitSize)};
			cb.complete.put (tuple2 (tok,  rsp2));
		     endmethod
		  endinterface
		  interface Get  get_flush_req = toGet (fifo_flush_reqs);
		  interface Put  put_flush_rsp = toPut (fifo_flush_rsps);
	       endinterface;

   let s_ifc = interface BC_MC_Server;
		  interface Put  put_rd_req;
		     method Action put (BC_MC_rd_req req);
			let cbtoken <- cb.reserve.get;
			let rdctl = ( (req.rdctl << tokenBitSize) | extend (pack (cbtoken)) );
			let req2  = BC_MC_rd_req { size:req.size, addr: req.addr, rdctl:rdctl};
			fifo_rd_reqs.enq (req2);
		     endmethod
		  endinterface
		  interface Put  put_wr_req = toPut (fifo_wr_reqs);
		  interface Get  get_rd_rsp = cb.drain;
		  interface Put  put_flush_req = toPut (fifo_flush_reqs);
		  interface Get  get_flush_rsp = toGet (fifo_flush_rsps);
       endinterface;

   return tuple2 (c_ifc, s_ifc);
endmodule: mkBC_MC_Port_Ordering_Transactor

// ----------------------------------------------------------------
// Vector of MC port ordering transactors: 8 MCs x even/odd

(* synthesize *)
module mkBC_Vec_MC_Port_Ordering_Transactors
           (Tuple2 #(Vector #(8, BC_MC_Client_Pair),
		     Vector #(8, BC_MC_Server_Pair)));

   Vector #(8, Tuple2 #(BC_MC_Client,
			BC_MC_Server))
       evens <- replicateM (mkBC_MC_Port_Ordering_Transactor);

   Vector #(8, Tuple2 #(BC_MC_Client,
			BC_MC_Server))
       odds <- replicateM (mkBC_MC_Port_Ordering_Transactor);

   return tuple2 (zip (map (tpl_1, evens), map (tpl_1, odds)),
		  zip (map (tpl_2, evens), map (tpl_2, odds)));
endmodule: mkBC_Vec_MC_Port_Ordering_Transactors

// ****************************************************************
// ****************************************************************
// ****************************************************************
// Dispatch Interface transactors

// ================================================================
// A simple dispatch transactor
//   - Implements AEGs
//   - Decodes AEG rd/wr instructions and executes them, provides AEG return-data
//   - Decodes CAEP0/1 instructions, forwards them to the app
//   - App signals completion of CAEP0/1 by returning an exception word (all 0s if no exception)

interface Simple_Dispatch_IFC;
   interface Get #(BC_Dispatch_inst)  get_caep_inst;
   interface Put #(BC_ExceptionVec)   put_caep_result;

   method BC_Data   rd_aeg (BC_Aeg_Idx  idx);
   method Action    wr_aeg (BC_Aeg_Idx  idx, BC_Data data);

   (* always_ready, always_enabled *)
   method Action    set_aeid (BC_AEId aeid);  // for $displays only
endinterface: Simple_Dispatch_IFC

(* synthesize *)
module mkSimple_Dispatch  #(parameter BC_Aeg_Idx aeg_idx_hi)
                          (Tuple2 #(BC_Dispatch_IFC,
				    Simple_Dispatch_IFC));

   Wire #(BC_AEId)                 aeid              <- mkBypassWire;
   Reg #(Bool)                     rg_caep_exec_busy <- mkReg (False);
   Reg #(BC_ExceptionVec)          rg_exception      <- mkReg (0);
   RegFile #(BC_Aeg_Idx, BC_Data)  rf_aegs           <- mkRegFile (0, aeg_idx_hi);

   FIFOF #(BC_Dispatch_inst)       fifo_insts              <- mkFIFOF;
   FIFOF #(BC_Dispatch_ret_data)   fifo_ret_data           <- mkFIFOF;
   FIFOF #(BC_ExceptionVec)        fifo_exceptions         <- mkFIFOF;
   FIFOF #(BC_Dispatch_inst)       fifo_caep_insts_to_app  <- mkFIFOF;

   rule rl_decode_and_exec_aeg_rd_wr (! rg_caep_exec_busy);
      // $display ("Simple_Dispatch[%0d].rl_decode: ", aeid, fshow (fifo_insts.first));
      let inst = fifo_insts.first.inst;
      let data  = fifo_insts.first.data;
      fifo_insts.deq;

      Bool             op_rd_aeg  = False;
      Bool             op_wr_aeg  = False;
      Bit #(18)        aeg_idx    = ?;
      Bool             op_caep    = False;
      BC_ExceptionVec  exception  = 0;

      // Decode the instruction
      if (inst [28:25] == 4'b1101) begin   // Format 4 instructions
	 if (inst [24:18] == 7'h40) begin
	    op_wr_aeg = True;
	    aeg_idx   = data [17:0];
	 end
	 else if (inst [24:18] == 7'h68) begin
	    op_rd_aeg = True;
	    aeg_idx   = data [17:0];
	 end
	 else if (inst [24:18] == 7'h70) begin
	    op_rd_aeg = True;
	    aeg_idx   = {6'h0, inst [17:6]};
	 end
	 else
	    exception = bc_exception_unimplemented_inst;
      end
      else if (inst [28:24] == 5'b11100) begin   // Format 5 instructions
	 if (inst [23:18] == 6'h18) begin
	    op_wr_aeg = True;
	    aeg_idx   = {6'h0, inst [17:12], inst [5:0]};
	 end
	 else if (inst [23:18] == 6'h20) begin
	    op_wr_aeg = True;
	    aeg_idx   = {6'b0, inst [17:12], inst [5:0]};
	 end
	 else
	    exception = bc_exception_unimplemented_inst;
      end
      else if (inst [28:24] == 5'b11101) begin   // Format 6 instructions
	 if (inst [23:18] == 6'h1C) begin
	    op_rd_aeg = True;
	    aeg_idx   = {6'h0, inst [17:6]};
	 end
	 else
	    exception = bc_exception_unimplemented_inst;
      end
      else if (inst [28:24] == 5'b11110) begin   // Format 7 instructions
	 if (inst [23] == 1'b1)	// CAEP instructions in range 20-3F
	    op_caep = True;
	 else
	    exception = bc_exception_unimplemented_inst;
      end
      else
	 exception = bc_exception_unimplemented_inst;

      // ----------------
      // Exec the instruction
      if (op_rd_aeg) begin
	 if (aeg_idx [5:0] <= aeg_idx_hi) begin
	    let rd_data = rf_aegs.sub (aeg_idx [5:0]);
	    fifo_ret_data.enq (rd_data);
	    // $display ("Simple_Dispatch[%0d].rl_decode: rd aeg [%0h] => %0h", aeid, aeg_idx [5:0], rd_data);
	 end
	 else
	    exception = bc_exception_invalid_aeg_idx;
      end
      else if (op_wr_aeg) begin
	 if (aeg_idx [5:0] <= aeg_idx_hi) begin
	    rf_aegs.upd (aeg_idx [5:0], data);
	    // $display ("Simple_Dispatch[%0d].rl_decode: wr aeg [%0h] <== %0h", aeid, aeg_idx [5:0], data);
	 end
	 else
	    exception = bc_exception_invalid_aeg_idx;
      end
      else if (op_caep) begin
	 // $display ("Simple_Dispatch[%0d].rl_decode: op caep", aeid);
	 fifo_caep_insts_to_app.enq (fifo_insts.first);
	 rg_caep_exec_busy <= True;
      end

      if (exception != 0) begin
	 fifo_exceptions.enq (exception);
	 // $display ("Simple_Dispatch[%0d].rl_decode: exception 0x%16h", aeid, exception);
      end
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   let dispatch_ifc
   = interface BC_Dispatch_IFC;
	interface Put             put_inst      = toPut (fifo_insts);
	interface Get             get_ret_data  = toGet (fifo_ret_data);
	interface Get             get_exception = toGet (fifo_exceptions);

	method    Bool            idle         = ((! rg_caep_exec_busy) && (! fifo_insts.notEmpty));
	method    BC_Aeg_Cnt_Rep  aeg_cnt      = 1 + extend (aeg_idx_hi);
     endinterface;

   let simple_dispatch_app_ifc
   = interface Simple_Dispatch_IFC;
	interface Get get_caep_inst   = toGet (fifo_caep_insts_to_app);

	interface Put put_caep_result;
	   method Action put (BC_ExceptionVec exception) if (rg_caep_exec_busy);
	      rg_caep_exec_busy <= False;
	      fifo_exceptions.enq (exception);
	      // $display ("Simple_Dispatch[%0d].put_caep_result: exception 0x%16h", aeid, exception);
	   endmethod
	endinterface

	method BC_Data rd_aeg (BC_Aeg_Idx  idx) = rf_aegs.sub (idx);

	method Action    wr_aeg (BC_Aeg_Idx  idx, BC_Data  data) = rf_aegs.upd (idx, data);

	method Action    set_aeid (BC_AEId aeid_val);  // for $displays only
	   aeid <= aeid_val;
	endmethod
     endinterface;

   return tuple2 (dispatch_ifc, simple_dispatch_app_ifc);
endmodule: mkSimple_Dispatch

// ****************************************************************
// ****************************************************************
// ****************************************************************
// Management Interface Transactors

// ================================================================
// A null management transactor
// Just passes aeid and csr_31_31_intlv_dis through,
// and loops back the management ring

interface BC_Management_App_IFC;
   (* always_ready *)
   method BC_AEId  aeid;

   (* always_ready *)
   method Bool  csr_31_31_intlv_dis;
endinterface

(* synthesize *)
module mkBC_Management_Null_Transactor  (Tuple2 #(BC_Management_IFC,
						  BC_Management_App_IFC));
   
   Wire #(BC_AEId)     wi_aeid                <- mkBypassWire;
   Wire #(Bool)        wi_csr_31_31_intlv_dis <- mkBypassWire;
   Wire #(BC_Ring_msg) wi_loopback            <- mkBypassWire;

   let mgmt_ifc
   = interface BC_Management_IFC;
	method Action set_aeid (BC_AEId aeid);
	   wi_aeid <= aeid;
	endmethod

	method Action m_csr_31_31_intlv_dis (Bool csr_31_31_intlv_dis);
	   wi_csr_31_31_intlv_dis <= csr_31_31_intlv_dis;
	endmethod

	interface Put put_ring_msg_in;
	   method Action put (BC_Ring_msg msg) = action wi_loopback <= msg; endaction;
	endinterface

	interface Get get_ring_msg_out;
	   method ActionValue #(BC_Ring_msg) get () = actionvalue return wi_loopback; endactionvalue;
	endinterface
     endinterface;

   let mgmt_app_ifc
   = interface BC_Management_App_IFC;
	method BC_AEId  aeid                = wi_aeid;
	method Bool     csr_31_31_intlv_dis = wi_csr_31_31_intlv_dis;
     endinterface;

   // ----------------
   // INTERFACE
   return tuple2 (mgmt_ifc, mgmt_app_ifc);
endmodule: mkBC_Management_Null_Transactor

// ================================================================

endpackage: BC_Transactors
