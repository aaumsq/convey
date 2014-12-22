// Copyright (c) 2011-2015 Bluespec, Inc.  All Rights Reserved.
// Distributed under license.

// Author: Rishiyur S. Nikhil

package BC_MC_Model;

// This package defines module 'mkBC_MC_Model' which is a model of
// a single Convey MC with even and odd ports.  It fields read, write,
// and flush requests from the BSV app and sends read and flush-complete
// responses to the BSV app.  There are 8 of these for each FPGA
// (thus, 32 total).  Reads and writes are simply passed through to a
// C function for execution in this simulation's process address
// space, i.e., it is assumed that these addresses refer to proper
// data structures created by the C app.

// NOTE: currently we only handle 64-bit reads/writes.

// NOTE: this model does not attempt to model latencies of memory in
// the real Convey HW, whether due to round-trip delays or contention

// ================================================================
// User controls for debugging (see 'config' method call)
// Note: config can be changed during execution

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
import Connectable  :: *;
import FShow        :: *;

// ----------------
// Bluespec Convey libs

import BC_HW_IFC      :: *;
import BC_BDPI_decls  :: *;

// ================================================================
// Interfaces

interface BC_MC_Model_IFC;
   interface BC_MC_Server_Pair  server_pair;

   // Admin
   method Action set_params (UInt #(16) latency,
			     UInt #(2) debug_mem_trace_verbosity,
			     Bool debug_stop_on_wrong_bank,
			     Bool debug_stop_on_unaligned);
   method Tuple2 #(UInt #(64), UInt #(64)) rd_wr_counts;
endinterface

// ================================================================

(* synthesize *)
module mkBC_MC_Model #(parameter UInt #(2) fpga, parameter UInt #(3) bank)
                     (BC_MC_Model_IFC);

   // customizations
   Reg #(UInt #(16))  rg_latency                   <- mkReg (2);
   Reg #(UInt #(2))   rg_debug_mem_trace_verbosity <- mkReg (1);
   Reg #(Bool)        rg_debug_stop_on_wrong_bank  <- mkReg (True);
   Reg #(Bool)        rg_debug_stop_on_unaligned   <- mkReg (True);

   // statistics
   Reg #(UInt #(64))  rg_n_reads_e  <- mkReg (0);
   Reg #(UInt #(64))  rg_n_reads_o  <- mkReg (0);

   Reg #(UInt #(64))  rg_n_writes_e <- mkReg (0);
   Reg #(UInt #(64))  rg_n_writes_o <- mkReg (0);

   // ----------------
   // For simulating latency
   Reg #(UInt #(64))  rg_cycle <- mkReg (0);

   rule rl_count_cycles;
      rg_cycle <= rg_cycle + 1;
   endrule

   // ----------------
   // Convey mem reqs are supposed to be directed to the correct memory bank (addr [8:6])
   // This function prints a warning if directed to the wrong bank.
   // Flag rg_debug_stop_on_wrong bank controls whether it just warns, or also exits.
   // Also checks that the address LSBs are consistent with the 'size' of request
   // Flag rg_debug_stop_on_unaligned controls whether it just warns, or also exits.

   function Action check_addr (BC_DataSize size, BC_Addr addr);
      action
	 if (addr [8:6] != pack (bank)) begin
	    if (rg_debug_stop_on_wrong_bank || (rg_debug_mem_trace_verbosity != 0))
	       $display ("WARNING: MC [fpga %0d][bank %0d] received request for MC [%0h] (addr = 0x%012h)",
			 fpga, bank, addr [8:6], addr);
	    if (rg_debug_stop_on_wrong_bank)
	       $finish (1);
	 end

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
   // This module is replicated for even and odd ports

   Integer  fifo_depth = 256;    // Max number of rd requests outstanding

   module mkM #(Reg #(UInt #(64)) rg_n_reads, Reg #(UInt #(64)) rg_n_writes) (BC_MC_Server);
      FIFOF #(BC_MC_rd_req)                        f_rd_reqs <- mkFIFOF;
      FIFOF #(Tuple2 #(BC_MC_rd_rsp, UInt #(64)))  f_rd_rsps <- mkSizedFIFOF (fifo_depth);
      FIFOF #(BC_MC_wr_req)                        f_wr_reqs <- mkSizedFIFOF (fifo_depth);
      FIFOF #(BC_MC_flush_req)                     f_flush_reqs <- mkFIFOF;
      FIFOF #(BC_MC_flush_rsp)                     f_flush_rsps <- mkFIFOF;

      rule rl_process_flush_reqs (! f_wr_reqs.notEmpty);
	 // Note: put_wr_req interface will stall if any flush is pending
	 // so we're guaranteed forward progress
	 f_flush_reqs.deq;
	 f_flush_rsps.enq (bc_mc_flush_rsp);
      endrule

      rule rl_process_rds;
	 let req = f_rd_reqs.first; f_rd_reqs.deq;
	 check_addr (req.size, req.addr);
	 let x <- bc_c_mem_read (extend (req.addr));    // Reads C memory
	 let rsp = BC_MC_rd_rsp {rdctl: req.rdctl, data: x};
	 f_rd_rsps.enq (tuple2 (rsp, rg_cycle + extend (rg_latency)));

	 rg_n_reads <= rg_n_reads + 1;
	 if (rg_debug_mem_trace_verbosity == 2)
	    $display ("Mem [%0d][%0d] rd ", fpga, bank, fshow (req), " -> ", fshow (rsp));
      endrule

      // The following disallows rds and wrs from firing together
      // (since Convey MC ports are shared for rd and wr requests
      (* preempts = "rl_process_rds, rl_process_wrs" *)

      rule rl_process_wrs;
	 let req = f_wr_reqs.first; f_wr_reqs.deq;
	 check_addr (req.size, req.addr);
	 bc_c_mem_write (extend (pack (req.size)), extend (req.addr), req.data);    // Writes C memory

	 rg_n_writes <= rg_n_writes + 1;
	 if (rg_debug_mem_trace_verbosity == 2)
	    $display ("Mem [%0d][%0d] wr ", fpga, bank, fshow (req));
      endrule

      // ----------------
      // INTERFACE

      interface put_rd_req    = toPut (f_rd_reqs);
      interface put_wr_req    = interface Put;
				   // Stall if any flush is pending
				   method Action put (BC_MC_wr_req req) if (! f_flush_reqs.notEmpty);
				      f_wr_reqs.enq (req);
				   endmethod
				endinterface;
      interface Get get_rd_rsp;
	 method ActionValue #(BC_MC_rd_rsp) get () if (tpl_2 (f_rd_rsps.first) <= rg_cycle);
	    f_rd_rsps.deq;
	    return tpl_1 (f_rd_rsps.first);
	 endmethod
      endinterface

      interface put_flush_req = toPut (f_flush_reqs);
      interface get_flush_rsp = toGet (f_flush_rsps);
   endmodule

   // ----------------------------------------------------------------
   // INTERFACE

   let ifc_e <- mkM (rg_n_reads_e, rg_n_writes_e);
   let ifc_o <- mkM (rg_n_reads_o, rg_n_writes_o);

   interface server_pair = tuple2 (ifc_e, ifc_o);

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
      return (tuple2 (rg_n_reads_e + rg_n_reads_o,
		      rg_n_writes_e + rg_n_writes_o));
   endmethod
endmodule

// ================================================================

endpackage
