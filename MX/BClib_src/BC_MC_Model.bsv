// Copyright (c) 2015 Bluespec, Inc.  All Rights Reserved.
// Distributed under license.

// Author: Rishiyur S. Nikhil

package BC_MC_Model;

// This package defines module 'mkBC_MC_Model' which is a model of a
// single Convey MC request/response port.  It receives memory
// requests from, and sends responses to the BSV app.  There are 16 of
// these for each of the 4 FPGAs (thus, 64 total).  Reads and writes
// are simply passed through to a C function for execution in this
// simulation's process address space, i.e., it is assumed that these
// addresses refer to proper data structures created by the C app.

// NOTE: currently we only handle 64-bit reads/writes.

// NOTE: although we model fixed memory latencies (delays), this
// cannot be an accurate model of latencies in the hardware which
// depend on specific hardware data paths, contention, etc.

// ================================================================
// User controls for debugging (see 'config' method call)
// Note: config is dynamic (can be changed during execution)

// ----------------
// UInt #(2) debug_mem_trace_verbosity
//   0:    quiet (no output)
//   1:    + print warnings for mem requests sent to wrong mem bank
//   2:    + print all memory traffic
// Default: 1

// ----------------
// Bool debug_stop_on_wrong_bank
//   True:   print warning message and kill simulation on a wrong bank request
//   False:  print warning message if memory trace verbosity >= 1, and continue
// Default: True
// Each Convey memory request (with addr A) is supposed to be sent only to memory bank A[8:6].
// On the real hardware, A[8:6] is ignored, so a "wrong-bank" request will return bogus aliased data.

// ----------------
// Bool debug_stop_on_unaligned
//   True:   print warning message and kill simulation on an unaligned request
//   False:  print warning message if memory trace verbosity >= 1, and continue
// Default: True
// Currently we only deal with 64-bit data (8-Byte).  Aligned accesses have the 3 lsbs of the address = 0.

// ----------------
// UInt#(16) latency    (in 150MHz cycles)
// Range 2..255
// Default: 2
// Simulated memory latency (should be >= 2)
// Typical observed HC-1 latencies are about 95 cycles
// For fastest simulation, use a latency of '2'

// ================================================================
// General BSV libs

import Vector       :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import FShow        :: *;

// ----------------
// Bluespec Convey libs

import BC_Utils       :: *;
import BC_HW_IFC      :: *;
import BC_BDPI_decls  :: *;

// ================================================================
// Interfaces

interface BC_MC_Model_IFC;
   interface BC_MC_Server  server;

   // Admin
   method Action set_params (UInt #(16) latency,
			     UInt #(2) debug_mem_trace_verbosity,
			     Bool debug_stop_on_wrong_bank,
			     Bool debug_stop_on_unaligned);
   method Tuple2 #(UInt #(64), UInt #(64)) rd_wr_counts;
endinterface

// ================================================================

(* synthesize *)
module mkBC_MC_Model #(parameter UInt #(2) fpga, parameter UInt #(4) bank)
                     (BC_MC_Model_IFC);

   // customizations
   Reg #(UInt #(16))  rg_latency                   <- mkReg (2);
   Reg #(UInt #(2))   rg_debug_mem_trace_verbosity <- mkReg (1);
   Reg #(Bool)        rg_debug_stop_on_wrong_bank  <- mkReg (True);
   Reg #(Bool)        rg_debug_stop_on_unaligned   <- mkReg (True);

   // Incoming requests
   FIFOF #(BC_MC_REQ)  f_reqs <- mkFIFOF;

   // ----------------
   // Outgoing responses.
   // For simulating latency: for each request processes, place its
   // response in f_rsps accompanied by the cycle before which it
   // should not be released.

   Reg #(UInt #(64))  rg_cycle <- mkReg (0);
   Integer  fifo_depth = 256;    // Max number of rd requests outstanding

   FIFOF #(Tuple2 #(BC_MC_RSP, UInt #(64)))  f_rsps <- mkSizedFIFOF (fifo_depth);

   // ----------------
   // Flush

   FIFOF #(BC_MC_flush_req)                    f_flush_reqs <- mkFIFOF;
   FIFOF #(BC_MC_flush_rsp)                    f_flush_rsps <- mkFIFOF;

   // ----------------
   // statistics
   Reg #(UInt #(64))  rg_n_reads  <- mkReg (0);
   Reg #(UInt #(64))  rg_n_writes <- mkReg (0);

   // ----------------
   // Checks that the address LSBs are consistent with the 'size' of request
   // Prints a warning on unaligned requests.
   // Flag rg_debug_stop_on_unaligned controls whether it just warns, or also exits.
   //
   // The following functionality is relevant for Convey HC platforms,
   // where mem requests are supposed to be directed to the correct
   // memory bank (addr [8:6]).  It may not be relevant any more for
   // MX and later, which always routes memory requests to the right
   // bank.
   // Prints a warning if directed to the wrong bank.
   // Flag rg_debug_stop_on_wrong bank controls whether it just warns, or also exits.

   function Action check_addr (BC_DataSize size, BC_Addr addr);
      action
     /*
	 if (addr [8:6] != pack (bank)[3:1]) begin
	    if (rg_debug_stop_on_wrong_bank || (rg_debug_mem_trace_verbosity != 0))
	       $display ("WARNING: MC [fpga %0d][bank %0d] received request for MC [%0h] (addr = 0x%012h)",
			 fpga, bank, addr [8:6], addr);
	    if (rg_debug_stop_on_wrong_bank)
	       $finish (1);
	 end
     */
	 Bool alignment_error = False;
	 case (size)
	    BC_2B: alignment_error = (addr[0] != 0);
	    BC_4B: alignment_error = (addr[1:0] != 0);
	    BC_8B: alignment_error = (addr[2:0] != 0);
	 endcase
	 if (alignment_error) begin
	    if (rg_debug_stop_on_unaligned || (rg_debug_mem_trace_verbosity != 0))
	       $display ("WARNING: MC [fpga %0d][bank %0d] address is not aligned: 0x%012h for size ",
			 fpga, bank, addr, fshow (size));
	    if (rg_debug_stop_on_unaligned)
	       $finish (1);
	 end
      endaction
   endfunction

   // ----------------
   // RULES

   rule rl_count_cycles;    // for simulating latency of reads/writes
      rg_cycle <= rg_cycle + 1;
   endrule

   rule rl_process_req;
      let req = f_reqs.first; f_reqs.deq;
      check_addr (req.len, req.vadr);

      BC_Data    x = ?;  Int #(64) ix = ?;
      BC_Data    y = ?;  Int #(64) iy = ?;
      BC_Data    z = ?;  Int #(64) iz = ?;

      BC_Data        rsp_data = ?;
      BC_MC_RSP_TYPE rsp_cmd = ?;
      case (req.cmd_sub)
	 REQ_RD:  action
		     rsp_data <- bc_c_mem_read (extend (pack (req.len)), extend (req.vadr));    // Reads C memory
		     rsp_cmd = RSP_RD_DATA;
		     rg_n_reads <= rg_n_reads + 1;
		  endaction
	 REQ_WR:  action
		     bc_c_mem_write (extend (pack (req.len)), extend (req.vadr), req.data);    // Writes C memory
		     rsp_cmd = RSP_WR_CMP;
		     rg_n_writes <= rg_n_writes + 1;
		  endaction
	 default: action
		     rsp_data <- bc_c_mem_atomic (extend (pack (req.cmd_sub)),    // Atomic op in C memory
						  extend (pack (req.len)),
						  extend (req.vadr),
						  req.data);
		     rsp_cmd = RSP_ATOMIC_DATA;
		  endaction
      endcase

      // Send response
      let rsp = BC_MC_RSP {cmd: rsp_cmd, sub: 0, rtnctl: req.rtnctl, data: rsp_data};
      f_rsps.enq (tuple2 (rsp, rg_cycle + extend (rg_latency)));

      if (rg_debug_mem_trace_verbosity == 2)
	 $display ("Mem [%0d][%0d] ", fpga, bank, fshow (req), " -> ", fshow (rsp));
   endrule

   rule rl_process_flush_reqs (! f_reqs.notEmpty);
      // Note: put_rq interface will stall if any flush is pending
      // so we're guaranteed forward progress
      f_flush_reqs.deq;
      f_flush_rsps.enq (bc_mc_flush_rsp);
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   interface BC_MC_Server server;
      interface req_rsp = interface Server;
			     interface Put request;
				// Stall if any flush is pending
				method Action put (BC_MC_REQ req) if (! f_flush_reqs.notEmpty);
				   f_reqs.enq (req);
				endmethod
			     endinterface
			     interface Get response;
				// Do not release response until specified target cycle (modeling latency)
				method ActionValue #(BC_MC_RSP) get () if (tpl_2 (f_rsps.first) <= rg_cycle);
				   f_rsps.deq;
				   return tpl_1 (f_rsps.first);
				endmethod
			     endinterface
			  endinterface;

      interface flush = fifofs_to_server (f_flush_reqs, f_flush_rsps);
   endinterface

   // Admin
   method Action set_params (UInt#(16) latency,
			     UInt #(2) debug_mem_trace_verbosity,
			     Bool debug_stop_on_wrong_bank,
			     Bool debug_stop_on_unaligned);
      rg_latency                   <= latency;
      rg_debug_mem_trace_verbosity <= debug_mem_trace_verbosity;
      rg_debug_stop_on_wrong_bank  <= debug_stop_on_wrong_bank;
      rg_debug_stop_on_unaligned   <= debug_stop_on_unaligned;
   endmethod

   method Tuple2 #(UInt #(64), UInt #(64)) rd_wr_counts;
      return (tuple2 (rg_n_reads, rg_n_writes));
   endmethod
endmodule

// ================================================================

endpackage
