// Copyright (c) 2015 Bluespec, Inc.  All Rights Reserved.
// Distributed under license.

// Author: Rishiyur S. Nikhil

// This package defines a module that encapsulates one Type 1 MC
// (memory channel) interface for each AE (Application Engine) FPGA on
// the Convey co-processor.  Each FPGA has 16 of these.

// The adapter converts between Convey Type 1 signal-level interface
// and BSV guarded FIFO interfaces.  It also aligns output data (e.g.,
// WR data) to the proper byte lanes if the request len is < 64b wide.

package BC_MC_Adapter;

// ================================================================
// BSV library imports

import Vector        :: *;
import FIFOF         :: *;
import GetPut        :: *;
import ClientServer  :: *;
import Connectable   :: *;
import DReg          :: *;
import SpecialFIFOs2 :: *;

// Local imports
import BC_Common  :: *;
import BC_HW_IFC  :: *;

// ================================================================
// Signal interface from BSV to the PDK for a single MC port
// Ref. Convey PDK Reference Manual v6.0, Figure 13 - Type 1 MX CAE to MC I/F Signal Interface

(* always_ready, always_enabled *)
interface BC_MC_Port_Signal_IFC;
   // Request Port
   method Bit #(1)   mc_req_vld;
   method Bit #(3)   mc_req_cmd;
   method Bit #(4)   mc_req_sub;
   method Bit #(2)   mc_req_len;
   method Bit #(48)  mc_req_vadr;
   method Bit #(32)  mc_req_rtnctl;
   method Bit #(64)  mc_req_data;
   method Action m_req_stall (Bit #(1) mc_req_stall);

   // Response Port
   method Action m_rsp (Bit #(1)  mc_rsp_vld,
			Bit #(3)  mc_rsp_cmd,
			Bit #(3)  mc_rsp_sub,
			Bit #(32) mc_rsp_rtnctl,
			Bit #(64) mc_rsp_data);
   method Bit #(1) mc_rsp_stall;

   // Write Flush
   method Bit #(1) mc_req_flush;
   method Action   m_rsp_flush_cmplt (Bit #(1) mc_rsp_flush_cmplt);
endinterface

// ================================================================
// The MC port adapter
// Takes a conventional BSV interface (BC_MC_Client) as arg
// and offers a signal-level interface (BC_MC_Port_Signal_IFC)
// which can be connected to a Verilog environment.

module mkBC_MC_port_adapter  #(BC_MC_Client  bsv_ifc)
                             (BC_MC_Port_Signal_IFC);

   // ----------------------------------------------------------------
   // Memory channel requests (output)
   Reg #(Bool)        rg_req_stall  <- mkDReg (False);
   Wire #(Bool)       wi_mc_req_vld <- mkDWire (False);
   Wire #(BC_MC_REQ)  wi_mc_req     <- mkDWire (?);

   rule rl_gen_req (! rg_req_stall);
      BC_MC_REQ req <- bsv_ifc.req_rsp.request.get;

      // For sub-8-byte writes, replicate the data for proper alignment
      BC_Data x = req.data;
      case (req.len)
	 BC_1B: x = { x[7:0], x[7:0], x[7:0], x[7:0], x[7:0], x[7:0], x[7:0], x[7:0] };
	 BC_2B: x = { x[15:0], x[15:0], x[15:0], x[15:0] };
	 BC_4B: x = { x[31:0], x[31:0] };
      endcase

      // req1 is the same as req, except with aligned data
      let req1 = BC_MC_REQ {cmd_sub: req.cmd_sub,
			    len:     req.len,
			    vadr:    req.vadr,
			    rtnctl:  req.rtnctl,
			    data:    x};
      wi_mc_req_vld <= True;
      wi_mc_req     <= req1;
   endrule

   // ----------------------------------------------------------------
   // Memory channel responses (input)
   // See mkRspFIFOF below for a special fifo that asserts 'full' (= stall) when there
   // is still enough space to accommodate the extra responses that come after asserting stall

   FIFOF #(BC_MC_RSP) fifo_rsps <- mkRspFIFOF;

   mkConnection (toGet (fifo_rsps), bsv_ifc.req_rsp.response);

   // ----------------------------------------------------------------
   // Flushes
   FIFOF #(BC_MC_flush_req)  fifo_flush_reqs            <- mkGFIFOF ((! ugenq), ugdeq);
   FIFOF #(BC_MC_flush_rsp)  fifo_flush_cmplts          <- mkGFIFOF (ugenq, (! ugdeq));
   Wire #(Bool)              wi_req_flush               <- mkDWire (False);
   Wire #(Bool)              wi_rsp_flush_cmplt         <- mkDWire (False);
   Reg #(UInt #(3))          rg_num_outstanding_flushes <- mkReg (0);

   rule rl_move_flush_reqs;
      let req <- bsv_ifc.flush.request.get;
      fifo_flush_reqs.enq (req);
   endrule

   rule rl_move_flush_rsps;
      fifo_flush_cmplts.deq;
      bsv_ifc.flush.response.put (?);
   endrule

   (* fire_when_enabled, no_implicit_conditions *)    // i.e., fires on every clock
   rule rl_flush;
      Bool req_flush;
      let num_outstanding_flushes = rg_num_outstanding_flushes;

      // Handle flush completions
      if (wi_rsp_flush_cmplt) begin
	 fifo_flush_cmplts.enq (?);
	 if (num_outstanding_flushes != 0)
	    num_outstanding_flushes = num_outstanding_flushes - 1;
	 else
	    $display ("ASSERTION_FAILURE: mkBC_MC_port_adapter.rl_flush: flush_cmplt, but num_outstanding_flushes is 0");
      end

      // Handle flush requests
      if ((num_outstanding_flushes < 4) && (fifo_flush_reqs.notEmpty)) begin
	 fifo_flush_reqs.deq;
	 req_flush = True;
	 num_outstanding_flushes = num_outstanding_flushes + 1;
      end
      else
	 req_flush = False;

      wi_req_flush <= req_flush;
      rg_num_outstanding_flushes <= num_outstanding_flushes;
   endrule

   // ----------------------------------------------------------------
   // Interface

   // Requests
   method Bit #(1)     mc_req_vld    = pack (wi_mc_req_vld);
   method Bit #(3)     mc_req_cmd    = pack (wi_mc_req.cmd_sub) [6:4];
   method Bit #(4)     mc_req_sub    = pack (wi_mc_req.cmd_sub) [3:0];
   method Bit #(2)     mc_req_len    = pack (wi_mc_req.len);
   method Bit #(48)    mc_req_vadr   = wi_mc_req.vadr;
   method Bit #(32)    mc_req_rtnctl = wi_mc_req.rtnctl;
   method Bit #(64)    mc_req_data   = wi_mc_req.data;

   method Action m_req_stall (Bit #(1) mc_req_stall);
      rg_req_stall <= unpack (mc_req_stall);
   endmethod

   // Responses
   method Action m_rsp (Bit #(1)  mc_rsp_vld,
			Bit #(3)  mc_rsp_cmd,
			Bit #(3)  mc_rsp_sub,
			Bit #(32) mc_rsp_rtnctl,
			Bit #(64) mc_rsp_data);
      if (mc_rsp_vld == 1'b1)
	 fifo_rsps.enq (BC_MC_RSP {cmd:    unpack (mc_rsp_cmd),
				   sub:    unpack (mc_rsp_sub),
				   rtnctl: mc_rsp_rtnctl,
				   data:   mc_rsp_data });
   endmethod

   // Response stalling: see mkRspFIFOF below for details
   method Bit #(1) mc_rsp_stall = pack (! fifo_rsps.notFull);

   // Flushes
   method Bit #(1)  mc_req_flush = pack (wi_req_flush);

   method Action m_rsp_flush_cmplt (Bit #(1) mc_rsp_flush_cmplt);
      wi_rsp_flush_cmplt <= unpack (mc_rsp_flush_cmplt);
   endmethod
endmodule

// ================================================================
// mkRspFIFOF is used for incoming memory responses
// Note: MC can send up to 8 additional responses after asserting 'stall'
// (PDK Ref Man v4.2, #9.3.3.4 says 5 more, but Convey said it's actually 8 more)
// Our buffer is 12 deep, and asserts 'full' (for 'stall') when 8 spaces are left.

(* synthesize *)
module mkRspFIFOF (FIFOF #(BC_MC_RSP));
   Bit #(4) dummy_size = ?;
   Bit #(8) dummy_headroom = ?;
   let ifc <- mkSizedFIFOF_AlmostFull (dummy_size, dummy_headroom, ugenq, (! ugdeq));
   return ifc;
endmodule

// ================================================================

endpackage
