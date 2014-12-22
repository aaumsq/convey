// Copyright (c) 2011-2015 Bluespec, Inc.  All Rights Reserved.
// Distributed under license.

// Author: Rishiyur S. Nikhil

// This package defines a module that encapsulates one MC (memory
// channel) interface for one AE (Application Engine) FPGA on the
// Convey HC-1 co-processor.
// Each FPGA has 16 of these (8 x even/odd).

// The adapter converts the Convey signal-level interface into BSV
// guarded FIFO interfaces.

package BC_MC_Adapter;

// ================================================================

// BSV library imports
import Vector        :: *;
import FIFOF         :: *;
// import FIFOLevel     :: *;    TODO: DELETE
import GetPut        :: *;
import Connectable   :: *;
import DReg          :: *;
import SpecialFIFOs2 :: *;

// Local imports
import BC_Common  :: *;
import BC_HW_IFC  :: *;

// ================================================================
// Signal interface

interface BC_MC_Port_Signal_IFC;
   (* always_ready *)
   method Bool            req_ld;
   (* always_ready *)
   method Bool            req_st;
   (* always_ready *)
   method BC_DataSize     req_size;
   (* always_ready *)
   method BC_Addr         req_vadr;
   (* always_ready *)
   method BC_Data         req_wrd_rdctl;
   (* always_ready, enable="rd_rq_stall" *)
   method Action          rd_rq_stall;
   (* always_ready, enable="wr_rq_stall" *)
   method Action          wr_rq_stall;

   (* always_ready, enable="rsp_push", prefix="" *)
   method Action rsp (BC_RdCtl rsp_rdctl, BC_Data rsp_data);
   (* always_ready *)
   method Bool rsp_stall;

   (* always_ready *)
   method Bool      req_flush;
   (* always_ready, enable="rsp_flush_cmplt" *)
   method Action    rsp_flush_cmplt;
endinterface

// ================================================================

// Mem read/write requests from adapter to Convey interface
typedef struct {
   Bool         req_ld;
   Bool         req_st;
   BC_DataSize  req_size;
   BC_Addr      req_vadr;
   BC_Data      req_wrd_rdctl;
   } BC_MC_req_signals
   deriving (Bits);

module mkBC_MC_port_adapter  #(BC_MC_Client  bsv_ifc)
                             (BC_MC_Port_Signal_IFC);

   // ----------------------------------------------------------------
   // Memory channel requests (output)
   Reg #(Bool)                rg_rd_rq_stall    <- mkDReg (False);
   Reg #(Bool)                rg_wr_rq_stall    <- mkDReg (False);
   Wire #(BC_MC_req_signals)  wi_mc_req_signals <- mkDWire (BC_MC_req_signals {req_ld:        False,
									       req_st:        False,
									       req_size:      ?,
									       req_vadr:      ?,
									       req_wrd_rdctl: ?});

   // Prioritize read reqs over write reqs, to minimize read latency
   (* descending_urgency = "rl_gen_rd_req, rl_gen_wr_req" *)

   rule rl_gen_rd_req (! rg_rd_rq_stall);
      let rd_req <- bsv_ifc.get_rd_req.get;
      let mc_req_signals = BC_MC_req_signals {req_ld:        True,
					      req_st:        False,
					      req_size:      rd_req.size,
					      req_vadr:      rd_req.addr,
					      req_wrd_rdctl: extend (rd_req.rdctl)};
      wi_mc_req_signals <= mc_req_signals;
   endrule

   rule rl_gen_wr_req (! rg_wr_rq_stall);
      let wr_req <- bsv_ifc.get_wr_req.get;

      // For sub-8-byte writes, replicate the data for proper alignment
      BC_Data x = wr_req.data;
      case (wr_req.size)
	 BC_1B: x = { x[7:0], x[7:0], x[7:0], x[7:0], x[7:0], x[7:0], x[7:0], x[7:0] };
	 BC_2B: x = { x[15:0], x[15:0], x[15:0], x[15:0] };
	 BC_4B: x = { x[31:0], x[31:0] };
      endcase

      let mc_req_signals = BC_MC_req_signals {req_ld:        False,
					      req_st:        True,
					      req_size:      wr_req.size,
					      req_vadr:      wr_req.addr,
					      req_wrd_rdctl: x};
      wi_mc_req_signals <= mc_req_signals;
   endrule

   // ----------------------------------------------------------------
   // Memory channel responses (input)
   // See mkRspFIFOF below for a special fifo that asserts 'full' (= stall) when there
   // is still enough space to accommodate the extra responses that come after asserting stall

   FIFOF #(BC_MC_rd_rsp) fifo_rd_rsps <- mkRspFIFOF;

   rule rl_move_rd_rsps;
      bsv_ifc.put_rd_rsp.put (fifo_rd_rsps.first); fifo_rd_rsps.deq;
   endrule

   // ----------------------------------------------------------------
   // Flushes
   FIFOF #(BC_MC_flush_req)  fifo_flush_reqs            <- mkGFIFOF ((! ugenq), ugdeq);
   FIFOF #(BC_MC_flush_rsp)  fifo_flush_cmplts          <- mkGFIFOF (ugenq, (! ugdeq));
   Wire #(Bool)              wi_req_flush               <- mkDWire (False);
   Wire #(Bool)              wi_rsp_flush_cmplt         <- mkDWire (False);
   Reg #(UInt #(3))          rg_num_outstanding_flushes <- mkReg (0);

   rule rl_move_flush_reqs;
      let req <- bsv_ifc.get_flush_req.get;
      fifo_flush_reqs.enq (req);
   endrule

   rule rl_move_flush_rsps;
      fifo_flush_cmplts.deq;
      bsv_ifc.put_flush_rsp.put (?);
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
   method Bool         req_ld        = wi_mc_req_signals.req_ld;
   method Bool         req_st        = wi_mc_req_signals.req_st;
   method BC_DataSize  req_size      = wi_mc_req_signals.req_size;
   method BC_Addr      req_vadr      = wi_mc_req_signals.req_vadr;
   method BC_Data      req_wrd_rdctl = wi_mc_req_signals.req_wrd_rdctl;

   method Action       rd_rq_stall   = action rg_rd_rq_stall <= True; endaction;
   method Action       wr_rq_stall   = action rg_wr_rq_stall <= True; endaction;

   // Responses
   method Action rsp (BC_RdCtl rsp_rdctl, BC_Data rsp_data);
      fifo_rd_rsps.enq (BC_MC_rd_rsp { rdctl: rsp_rdctl, data: rsp_data });
   endmethod

   // Response stalling: see mkRspFIFOF below for details
   method Bool rsp_stall = (! fifo_rd_rsps.notFull);

   // Flushes
   method Bool      req_flush       = wi_req_flush;
   method Action    rsp_flush_cmplt = action wi_rsp_flush_cmplt <= True; endaction;
endmodule

// ================================================================
// The following are needed only for purposes of fixing up the names
// of the generated Verilog signals to match Convey specs.  If there
// is some other way to match Verilog names, then these are
// unnecessary.

// ----------------------------------------------------------------
// Signal interface for "even" ports, to get a "_e" suffix
// This is identical to BC_MC_Signal_IFC (above) except for the _e suffix.

interface BC_MC_Port_Signal_IFC_e;
   (* always_ready *)
   method Bool            req_ld_e;
   (* always_ready *)
   method Bool            req_st_e;
   (* always_ready *)
   method BC_DataSize     req_size_e;
   (* always_ready *)
   method BC_Addr         req_vadr_e;
   (* always_ready *)
   method BC_Data         req_wrd_rdctl_e;
   (* always_ready, enable="rd_rq_stall_e" *)
   method Action          rd_rq_stall_e;
   (* always_ready, enable="wr_rq_stall_e" *)
   method Action          wr_rq_stall_e;

   (* always_ready, enable="rsp_push_e", prefix="" *)
   method Action rsp_e (BC_RdCtl rsp_rdctl_e, BC_Data rsp_data_e);
   (* always_ready *)
   method Bool rsp_stall_e;

   (* always_ready *)
   method Bool      req_flush_e;
   (* always_ready, enable="rsp_flush_cmplt_e" *)
   method Action    rsp_flush_clmplt_e;
endinterface

// ----------------
// This function converts from the sans-suffix version to the with-suffix version.

function BC_MC_Port_Signal_IFC_e  to_mc_port_e (BC_MC_Port_Signal_IFC  ifc)
   = interface BC_MC_Port_Signal_IFC_e;
	method Bool         req_ld_e        = ifc.req_ld;
	method Bool         req_st_e        = ifc.req_st;
	method BC_DataSize  req_size_e      = ifc.req_size;
	method BC_Addr      req_vadr_e      = ifc.req_vadr;
	method BC_Data      req_wrd_rdctl_e = ifc.req_wrd_rdctl;
	method Action       rd_rq_stall_e   = ifc.rd_rq_stall;
	method Action       wr_rq_stall_e   = ifc.wr_rq_stall;

	method Action rsp_e (BC_RdCtl rsp_rdctl_e, BC_Data rsp_data_e) = ifc.rsp (rsp_rdctl_e, rsp_data_e);
	method Bool   rsp_stall_e                                      = ifc.rsp_stall;

	method Bool      req_flush_e        = ifc.req_flush;
	method Action    rsp_flush_clmplt_e = ifc.rsp_flush_cmplt;
     endinterface;

// ----------------------------------------------------------------
// Signal interface for "odd" ports, to get a "_o" suffix
// This is identical to BC_MC_Port_Signal_IFC (above) except for the _o suffix.

interface BC_MC_Port_Signal_IFC_o;
   (* always_ready *)
   method Bool            req_ld_o;
   (* always_ready *)
   method Bool            req_st_o;
   (* always_ready *)
   method BC_DataSize     req_size_o;
   (* always_ready *)
   method BC_Addr         req_vadr_o;
   (* always_ready *)
   method BC_Data         req_wrd_rdctl_o;
   (* always_ready, enable="rd_rq_stall_o" *)
   method Action          rd_rq_stall_o;
   (* always_ready, enable="wr_rq_stall_o" *)
   method Action          wr_rq_stall_o;

   (* always_ready, enable="rsp_push_o", prefix="" *)
   method Action rsp_o (BC_RdCtl rsp_rdctl_o, BC_Data rsp_data_o);
   (* always_ready *)
   method Bool rsp_stall_o;

   (* always_ready *)
   method Bool      req_flush_o;
   (* always_ready, enable="rsp_flush_cmplt_o" *)
   method Action    rsp_flush_clmplt_o;
endinterface

// ----------------
// This function converts from the sans-suffix version to the with-suffix version.

function BC_MC_Port_Signal_IFC_o  to_mc_port_o (BC_MC_Port_Signal_IFC  ifc)
   = interface BC_MC_Port_Signal_IFC_o;
	method Bool         req_ld_o        = ifc.req_ld;
	method Bool         req_st_o        = ifc.req_st;
	method BC_DataSize  req_size_o      = ifc.req_size;
	method BC_Addr      req_vadr_o      = ifc.req_vadr;
	method BC_Data      req_wrd_rdctl_o = ifc.req_wrd_rdctl;
	method Action       rd_rq_stall_o   = ifc.rd_rq_stall;
	method Action       wr_rq_stall_o   = ifc.wr_rq_stall;

	method Action rsp_o (BC_RdCtl rsp_rdctl_o, BC_Data rsp_data_o) = ifc.rsp (rsp_rdctl_o, rsp_data_o);
	method Bool   rsp_stall_o                                      = ifc.rsp_stall;

	method Bool      req_flush_o        = ifc.req_flush;
	method Action    rsp_flush_clmplt_o = ifc.rsp_flush_cmplt;
     endinterface;

// ================================================================
// mkRspFIFOF is used for incoming memory responses
// Note: MC can send up to 8 additional responses after asserting 'stall'
// (PDK Ref Man v4.2, #9.3.3.4 says 5 more, but Convey said it's actually 8 more)
// Our buffer is 12 deep, and asserts 'full' (for 'stall') when 8 spaces are left.

(* synthesize *)
module mkRspFIFOF (FIFOF #(BC_MC_rd_rsp));
   Bit #(4) dummy_size = ?;
   Bit #(8) dummy_headroom = ?;
   let ifc <- mkSizedFIFOF_AlmostFull (dummy_size, dummy_headroom, ugenq, (! ugdeq));
   return ifc;
endmodule

// ================================================================

endpackage
