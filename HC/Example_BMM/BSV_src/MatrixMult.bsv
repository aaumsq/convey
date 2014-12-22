// Copyright (c) 2013-2015 Bluespec, Inc. All Rights Reserved.
//
// Author: Rishiyur Nikhil, Bluespec, Inc.

package MatrixMult ;

// ================================================================

// This package defines a memory-to-memory Matrix Multiply module
// containing multiple parallel inner-product engines and parallel ports to memory

// Terminology and general information:

// Basic data elements are 64-bit scalars (here signed ints, but easy
// to change to floating point).

// Matrices are A[m,n] x B[n,p] => C[m,p]    (dimensions in scalar elements)
// We use 'i' indexes along the 'm' dimension (in A and C).
// We use 'j' indexes along the 'n' dimension (in A and B).
// We use 'k' indexes along the 'p' dimension (in B and C).

// This implementation is a "blocked" implementation, i.e.,
// - the matrices are structured into blocks
//     - A[mb,nb] blocks, each of size mm x nn scalars
//     - B[nb,pb] blocks, each of size nn x pp scalars
//     - C[mb,pb] blocks, each of size mm x pp scalars
// - the code reads and write a block at a time from memory to FPGA

// - the core 'multiply-add' function, C[i,j] = C[i,j]+A[i,k]B[k,j]
//     is performed on complete blocks, not individual scalars, i.e.,
//     '*' is a matix-multiply on blocks of size [mm,nn]x[nn,pp] => [mm,pp]
//     '+' is a matrix-add on blocks of size [mm,pp]

// The concurrency strategy is to generate a single shared queue of
// 'tasks', where each task is a complete blocked inner-product.  A
// parameterized number (N_Engines) of 'inner-product engines' consume
// and execute these tasks concurrently.  Each engine is itself a
// 3-stage pipeline: a 'reader' to generate read requests for blocks
// from A and B, a 'MAC loop' that fields the responses and does the
// multiply-accumulate of the inner-product value C, and a 'writer'
// that writes block C back to memory.

// To use 4 FPGAs concurrently, one could slice the 'A' and 'C'
// matrices horizontally (giving ~m/4 rows to each slice); then
//   FPGA 0 multiplies A[ 0  .. m/4,n] x B[n,p] => C[ 0  .. m/4,p]
//   FPGA 1 multiplies A[ m/4.. m/2,n] x B[n,p] => C[ m/4.. m/2,p]
//   FPGA 2 multiplies A[ m/2..3m/4,n] x B[n,p] => C[ m/2..3m/4,p]
//   FPGA 3 multiplies A[3m/4.. m  ,n] x B[n,p] => C[3m/4.. m,  p]
// i.e., they are just given different arguments representing these
// slices.

// Obviously, different concurrency and performance will be achieved
// by choosing different values for 'N_Engines', size of blocks,
// layouts of blocks-in-matrix and scalars-in-block, etc.  The "best"
// choice should be determined by experiment.

// The code makes no assumptions about actual memory layout (such as
// row-major, column-major, etc).  Each of A, B and C can have
// different layouts. The, layout of blocks-in-matrix can be different
// from layout of scalars-in-block.  This abtraction is achieved by
// having the caller (host code) provide relevant address-increments
// as arguments (see below for details).

// Since we cannot predict which MC will hold each dataum, we use a
// Memory Network to route memory requests and responses properly.

// Memory re-ordering is handled inside each inner-product engine,
// since its reads may distribute to different memory banks and
// responses may return out of order.

// The program's single input is 'pArgv', the address of an argument
// block which contains all the real arguments:

Integer arg_argc = 0;    // # of args, including this one (should contain 22)

Integer arg_pA   = 1;    // address of A[0,0]
Integer arg_pB   = 2;    // address of B[0,0] 
Integer arg_pC   = 3;    // address of C[0,0] 

Integer arg_mb   = 4;    // matrix dimension (# of blocks)
Integer arg_nb   = 5;    // matrix dimension (# of blocks)
Integer arg_pb   = 6;    // matrix dimension (# of blocks)

Integer arg_mm   = 7;    // block dimension (# of elements)
Integer arg_nn   = 8;    // block dimension (# of elements)
Integer arg_pp   = 9;    // block dimension (# of elements)

Integer arg_dAi1 = 10;   // address increment in A along i by 1 element
Integer arg_dAk1 = 11;   // address increment in A along k by 1 element
Integer arg_dAib = 12;   // address increment in A along i by 1 block
Integer arg_dAkb = 13;   // address increment in A along k by 1 block

Integer arg_dBk1 = 14;   // address increment in B along k by 1 element
Integer arg_dBj1 = 15;   // address increment in B along j by 1 element
Integer arg_dBkb = 16;   // address increment in B along k by 1 block
Integer arg_dBjb = 17;   // address increment in B along j by 1 block

Integer arg_dCi1 = 18;   // address increment in C along i by 1 element
Integer arg_dCj1 = 19;   // address increment in C along j by 1 element
Integer arg_dCib = 20;   // address increment in C along i by 1 block
Integer arg_dCjb = 21;   // address increment in C along j by 1 block

typedef 22 NUM_ARGS;

// # of inner-product engines that run concurrently
typedef 8 N_Engines;

// ================================================================
// BSV library imports

import Vector       :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;

// ----------------
// BC library imports

import BC_Utils           :: *;
import BC_HW_IFC          :: *;

// ================================================================
// Project imports

import InnerProduct :: *;

// ================================================================
// The interface for the MatrixMult module
// has one Server port on which it handles configuration requests from the CPU

interface MatrixMult_IFC;
   method Action start (BC_AEId fpga_id, BC_Addr pArgv);
   method Action waitTillDone;

   interface Vector #(8, BC_MC_Client_Pair) mc_ifcs;
endinterface  

// ================================================================
// The MatrixMult module

// The uid parameter allows unique identification of multiple copies
// of this module

(* synthesize *)
module mkMatrixMult (MatrixMult_IFC);

   Integer verbosity = 1;

   if (valueOf (N_Engines) > 8)
      errorM ("ERROR: N_Engines should be <= 8");

   // ----------------------------------------------------------------
   // Local copy of args (retrieved from argv block in memory)

   Reg #(BC_AEId) rg_fpga_id <- mkRegU;
   Reg #(BC_Addr) rg_pArgv   <- mkRegU;

   Vector #(NUM_ARGS, Reg #(BC_Addr)) argv <- replicateM (mkRegU);

   // Mem requests and responses for MC[7]
   // Initially _o port is  used for argv reads,
   // then used by inner-product engine[7]
   FIFOF #(BC_Addr)                     f_rd_reqs_e_7 <- mkFIFOF;
   FIFOF #(BC_Data)                     f_rd_rsps_e_7 <- mkFIFOF;
   FIFOF #(Tuple2 #(BC_Addr, BC_Data))  f_wr_reqs_e_7 <- mkFIFOF;

   FIFOF #(BC_Addr)                     f_rd_reqs_o_7 <- mkFIFOF;
   FIFOF #(BC_Data)                     f_rd_rsps_o_7 <- mkFIFOF;
   FIFOF #(Tuple2 #(BC_Addr, BC_Data))  f_wr_reqs_o_7 <- mkFIFOF;

   // ----------------------------------------------------------------
   // Section: inner-product engines driven from a shared task queue

   Vector #(N_Engines, InnerProduct_IFC) vips <- replicateM (mkInnerProduct);

   // Shared task queue.  Each task does one blocked inner-product.
   // Task args are ptrs to blocked-vector A, blocked-vector B, block C
   FIFOF #(Tuple3 #(BC_Addr,BC_Addr,BC_Addr)) f_ip_tasks <- mkFIFOF;

   // Task count (total mb x pb tasks)
   Reg #(Bit #(32)) rg_mb_pb         <- mkRegU;
   Vector #(N_Engines, Reg #(Bit #(32))) vr_n_tasks_started <- replicateM (mkRegU);

   // Task distributor
   for (Integer w = 0; w < valueOf (N_Engines); w = w + 1)
      rule rl_distribute_task_to_engine;
	 match { .pAi0, .pB0j, .pCij } = f_ip_tasks.first; f_ip_tasks.deq;
	 vips[w].start (pAi0, pB0j, pCij);
	 vr_n_tasks_started[w] <= vr_n_tasks_started[w] + 1;

	 if (verbosity != 0)
	    $display ("%0d: MatrixMult_%0d: engine[%0d] starting task %0h %0h %0h", cur_cycle, rg_fpga_id,
		      w, pAi0, pB0j, pCij);
      endrule

   // ----------------
   // All done when mb x pb tasks have been started, and all ip
   // engines have stopped running.  Note, rg_all_done goes True
   // 1 cycle after rg_all_tasks_started and rg_no_vips_running,
   // but that's acceptable.

   Reg #(Bool) rg_all_tasks_started <- mkRegU;
   Reg #(Bool) rg_no_vips_running <- mkRegU;
   Reg #(Bool) rg_all_done <- mkReg (False);

   function Bool not_running (Integer w) = (! vips[w].running);

   rule rl_all_done (! rg_all_done);
      Bit #(32) tot_tasks_started = fold (\+  , readVReg (vr_n_tasks_started));
      rg_all_tasks_started <= (tot_tasks_started == rg_mb_pb);

      Vector #(N_Engines, Bool) v_not_running = genWith (not_running);
      rg_no_vips_running <= fold (\&&  , v_not_running);

      Bool all_done = (rg_all_tasks_started && rg_no_vips_running);
      rg_all_done <= all_done;

      if (all_done && (verbosity > 0))
	 $display ("%0d: MatrixMult_%0d: all %0d tasks completed", cur_cycle, rg_fpga_id, tot_tasks_started);
   endrule

   // ----------------------------------------------------------------
   // Section: core matrix multiplication behavior

   // Program counters to sequence rules below
   Reg #(Bit #(4)) pc    <- mkReg (0);
   Reg #(Bit #(4)) pc1   <- mkReg (0);

   Reg #(BC_Addr) rg_i   <- mkRegU;
   Reg #(BC_Addr) rg_j   <- mkRegU;
   Reg #(BC_Addr) rg_Ai0 <- mkRegU;
   Reg #(BC_Addr) rg_B0j <- mkRegU;
   Reg #(BC_Addr) rg_Ci0 <- mkRegU;
   Reg #(BC_Addr) rg_Cij <- mkRegU;

   Reg #(Bit #(6)) rg_w <- mkRegU;

   rule rl_init (pc == 1);
      writeVReg (vr_n_tasks_started, replicate (0));
      rg_all_done <= False;
      argv [arg_argc] <= fromInteger (valueOf (NUM_ARGS));    // Could also read this from memory at pArgv[0]

      rg_i <= 1; pc  <= 2;
      rg_j <= 1; pc1 <= 2;
   endrule

   rule rl_argv_request (pc == 2);
      BC_Addr  a = extend (rg_i);
      BC_Addr  addr  = rg_pArgv + (a << 3);
      f_rd_reqs_o_7.enq (addr);
      rg_i <= rg_i + 1;

      if (rg_i == (argv[arg_argc] - 1))
	 pc <= 3;
   endrule

   rule rl_argv_response (pc1 == 2);
      let d = f_rd_rsps_o_7.first;  f_rd_rsps_o_7.deq;
      argv [rg_j] <= truncate (d);
      rg_j <= rg_j + 1;

      if (rg_j == (argv[arg_argc] - 1))
	 pc1 <= 3;
   endrule

   rule rl_got_argv ((pc == 3) && (pc1 == 3));
      // TODO: the foll. is to avoid 48-bit (BC_Addr) multiplication, for timing closure
      // Assumes mb < 2^24 and pb < 2^24 (# of blocks in m and p dimensions)
      Bit #(24) mb24 = truncate (argv[arg_mb]);
      Bit #(24) pb24 = truncate (argv[arg_pb]);
      Bit #(24) mb_pb_24 = mb24 * pb24;
      rg_mb_pb <= extend (mb_pb_24);

      rg_Ai0 <= argv[arg_pA];
      rg_B0j <= argv[arg_pB];
      rg_Ci0 <= argv[arg_pC];
      rg_Cij <= argv[arg_pC];
      rg_i   <= 0;
      rg_j   <= 0;
      rg_w   <= 0;
      pc     <= 4;

      if (verbosity > 0) begin
	 $display ("%0d: MatrixMult_%0d: starting: &A=%0h x &B=%0h => &C=%0h, mb=%0d, nb=%0d, pb=%0d",
		   cur_cycle, rg_fpga_id,
		   argv[arg_pA], argv[arg_pB], argv[arg_pC],
		   argv[arg_mb], argv[arg_nb], argv[arg_pb]);
	 $display ("            mm=%0d, nn=%0d, pp=%0d", argv[arg_mm], argv[arg_nn], argv[arg_pp]);
	 $display ("            dAi1=%0h, dAk1=%0h, dAib=%0h, dAkb=%0h",
		   argv[arg_dAi1], argv[arg_dAk1], argv[arg_dAib], argv[arg_dAkb]);
	 $display ("            dBk1=%0h, dBj1=%0h, dBkb=%0h, dBjb=%0h",
		   argv[arg_dBk1], argv[arg_dBj1], argv[arg_dBkb], argv[arg_dBjb]);
	 $display ("            dCi1=%0h, dCj1=%0h, dCib=%0h, dCjb=%0h",
		   argv[arg_dCi1], argv[arg_dCj1], argv[arg_dCib], argv[arg_dCjb]);
      end
   endrule

   Bit #(8) uid = { extend (rg_fpga_id), 4'h0 };

   rule rl_init_vips (pc == 4);
      vips[rg_w].init ((uid | extend (rg_w)),
		       truncate (argv[arg_mm] - 1),
		       truncate (argv[arg_nn] - 1),
		       truncate (argv[arg_pp] - 1),
		       argv[arg_nb] - 1,
		       argv[arg_dAi1], argv[arg_dAk1], argv[arg_dAkb],
		       argv[arg_dBk1], argv[arg_dBj1], argv[arg_dBkb],
		       argv[arg_dCi1], argv[arg_dCj1]);

      if (rg_w != fromInteger (valueOf (N_Engines) - 1))
	 rg_w <= rg_w + 1;
      else
	 pc <= 5;
   endrule

   rule rl_gen_vip_tasks (pc == 5);
      f_ip_tasks.enq (tuple3 (rg_Ai0, rg_B0j, rg_Cij));

      if (rg_j != argv [arg_pb] - 1) begin
	 rg_j <= rg_j + 1;
	 rg_B0j <= rg_B0j + argv[arg_dBjb];
	 rg_Cij <= rg_Cij + argv[arg_dCjb];
      end
      else if (rg_i != argv [arg_mb]-1) begin
	 rg_j <= 0;
	 rg_i <= rg_i + 1;
	 rg_Ai0 <= rg_Ai0 + argv[arg_dAib];
	 rg_B0j <= argv[arg_pB];
	 rg_Ci0 <= rg_Ci0 + argv[arg_dCib];
	 rg_Cij <= rg_Ci0 + argv[arg_dCib];
      end
      else
	 pc <= 6;
   endrule

   rule rl_wait_for_completion ((pc == 6) && rg_all_done);
      pc  <= 0;
      pc1 <= 0;
   endrule

   // ----------------
   // Forward vips[7] mem requests and responses to/from MC[7] only
   // if there is a vips [7] (N_Engines == 8), and
   // after argv has been read (pc1 > 2) and computation has started.

   if (valueOf (N_Engines) > 7) begin
      mkConnection (vips [7].rd_client_e.request, toPut (f_rd_reqs_e_7));
      mkConnection (vips [7].rd_client_o.request, toPut (f_rd_reqs_o_7));

      mkConnection (toGet (f_rd_rsps_e_7), vips [7].rd_client_e.response);
      rule rl_connect7 (pc1 > 2);
	 let d = f_rd_rsps_o_7.first; f_rd_rsps_o_7.deq;
	 vips [7].rd_client_o.response.put (d);
      endrule

      mkConnection (vips [7].wr_client_e, toPut (f_wr_reqs_e_7));
      mkConnection (vips [7].wr_client_o, toPut (f_wr_reqs_o_7));
   end

   // ----------------------------------------------------------------
   // INTERFACE

   function Get #(BC_MC_rd_req)  to_get_rd_req (Get #(BC_Addr) g);
      return interface Get;
		method ActionValue #(BC_MC_rd_req) get;
		   let a <- g.get;
		   return BC_MC_rd_req {size: BC_8B, addr: a, rdctl: 0};
		endmethod
	     endinterface;
   endfunction

   function Put #(BC_MC_rd_rsp)  to_put_rd_rsp (Put #(BC_Data) p);
      return interface Put;
		method Action put (BC_MC_rd_rsp rsp);
		   p.put (rsp.data);
		endmethod
	     endinterface;
   endfunction

   function Get #(BC_MC_wr_req)  to_get_wr_req (Get #(Tuple2 #(BC_Addr, BC_Data)) g);
      return interface Get;
		method ActionValue #(BC_MC_wr_req) get;
		   match { .a, .d } <- g.get;
		   return BC_MC_wr_req {size: BC_8B, addr: a, data: d};
		endmethod
	     endinterface;
   endfunction

   Vector #(8, BC_MC_Client_Pair) v_ifcs;
   for (Integer mc = 0; mc < 7; mc = mc + 1) begin
      if (mc < valueOf (N_Engines))
	 v_ifcs [mc] = tuple2 (interface BC_MC_Client;
				  interface get_rd_req = to_get_rd_req (vips [mc].rd_client_e.request);
				  interface put_rd_rsp = to_put_rd_rsp (vips [mc].rd_client_e.response);
				  interface get_wr_req = to_get_wr_req (vips [mc].wr_client_e);
				  interface get_flush_req = getstub;
				  interface put_flush_rsp = putstub;
			       endinterface,
			       interface BC_MC_Client;
				  interface get_rd_req = to_get_rd_req (vips [mc].rd_client_o.request);
				  interface put_rd_rsp = to_put_rd_rsp (vips [mc].rd_client_o.response);
				  interface get_wr_req = to_get_wr_req (vips [mc].wr_client_o);
				  interface get_flush_req = getstub;
				  interface put_flush_rsp = putstub;
			       endinterface);
      else
	 v_ifcs [mc] = tuple2 (interface BC_MC_Client;
				  interface get_rd_req = getstub;
				  interface put_rd_rsp = putstub;
				  interface get_wr_req = getstub;
				  interface get_flush_req = getstub;
				  interface put_flush_rsp = putstub;
			       endinterface,
			       interface BC_MC_Client;
				  interface get_rd_req = getstub;
				  interface put_rd_rsp = putstub;
				  interface get_wr_req = getstub;
				  interface get_flush_req = getstub;
				  interface put_flush_rsp = putstub;
			       endinterface);
   end

   // MC [7] is used for the argv reads
   v_ifcs [7] = tuple2 (interface BC_MC_Client;
			   interface get_rd_req = to_get_rd_req (toGet (f_rd_reqs_e_7));
			   interface put_rd_rsp = to_put_rd_rsp (toPut (f_rd_rsps_e_7));
			   interface get_wr_req = to_get_wr_req (toGet (f_wr_reqs_e_7));
			   interface get_flush_req = getstub;
			   interface put_flush_rsp = putstub;
			endinterface,
			interface BC_MC_Client;
			   interface get_rd_req = to_get_rd_req (toGet (f_rd_reqs_o_7));
			   interface put_rd_rsp = to_put_rd_rsp (toPut (f_rd_rsps_o_7));
			   interface get_wr_req = to_get_wr_req (toGet (f_wr_reqs_o_7));
			   interface get_flush_req = getstub;
			   interface put_flush_rsp = putstub;
			   endinterface);

   method Action start (BC_AEId fpga_id, BC_Addr pArgv);
      rg_fpga_id <= fpga_id;
      rg_pArgv <= pArgv;
      pc <= 1;
   endmethod

   method Action waitTillDone () if (pc == 0);
      noAction;
   endmethod

   interface mc_ifcs = v_ifcs;
endmodule

// ================================================================

endpackage: MatrixMult
