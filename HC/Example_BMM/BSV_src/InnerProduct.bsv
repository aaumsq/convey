// Copyright (c) 2013-2015 Bluespec, Inc. All Rights Reserved.
//
// Author: Rishiyur Nikhil, Bluespec, Inc.

package InnerProduct;

// ================================================================
// This package defines a memory-to-memory "blocked" inner-product module.
// If the inputs are:
//    A    a vector of nb blocks, each of size mm x nn
//    B    a vector of nb blocks, each of size nn x pp
// the output is:
//    C    a block of size mm x pp
//    representing the "blocked" inner product of A and B
//    i.e., in the classical definition: C = sigma+ (A[j] x B[j])
// we interpret:
//    j    as a block index,
//    +    as matrix-add of two mm x pp blocks
//    *    as matrix-multiply of a (mm x nn) block with a (nn x pp) block
// Within each block, the basic data elements are 64-bit scalars
// (here integers, but easy to replace this with floating point)

// The code makes no assumptions about memory layout (row-major,
// column-major, other).  Further, layout of blocks-in-vector can be
// different from layout of data-in-blocks.  This abtraction is
// achieved by having the caller provide relevant address-increments
// as arguments (see below for details).

// The 'reset' method sets up the following parameters (which may
// be re-used across multiple inner-product computations):
//    mm, nn, pp          # elements in blocks
//    nb                  # of blocks in each vector A and B
//    dAi1, dAk1, dAkb    addr incr for A in i/k direction by 1 elem/1 block
//    dBk1, dBj1, dBkb    addr incr for B in k/j direction by 1 elem/1 block
//    dCi1, dCj1          addr incr for C in i/j direction by 1 elem

// The 'start' method is used for each inner-product computation. Args:
//    pA: addr of blocked vector A (nb blocks, each mm x nn)
//    pB: addr of blocked vector B (nb blocks, each nn x pp)
//    pC: addr of C output block (mm x pp)

// ================================================================
// Bluespec library imports

import Vector       :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import StmtFSM      :: *;

// ----------------
// BC library imports

import BC_Utils           :: *;
import BC_HW_IFC          :: *;

// ----------------
// Local imports

import BlockMAC :: *;

// ================================================================
// The interface for the InnerProduct module

interface InnerProduct_IFC;
   method Action init (Bit #(8) uid_arg,
		       Bit#(4) mm1_arg, Bit#(4) nn1_arg, Bit#(4) pp1_arg,
		       BC_Addr nb1_arg,
		       BC_Addr dAi1_arg, BC_Addr dAk1_arg, BC_Addr dAkb_arg,
		       BC_Addr dBk1_arg, BC_Addr dBj1_arg, BC_Addr dBkb_arg,
		       BC_Addr dCi1_arg, BC_Addr dCj1_arg);

   method Action start (BC_Addr pA_arg, BC_Addr pB_arg, BC_Addr pC_arg);

   method Bool running;

   // The memory ports
   interface Client #(BC_Addr, BC_Data)        rd_client_e;
   interface Client #(BC_Addr, BC_Data)        rd_client_o;
   interface Get #(Tuple2 #(BC_Addr, BC_Data)) wr_client_e;
   interface Get #(Tuple2 #(BC_Addr, BC_Data)) wr_client_o;
endinterface

// ================================================================
// The InnerProduct module

(* synthesize *)
module mkInnerProduct (InnerProduct_IFC);

   Integer verbosity = 0;    // Increase this to get more $display debugging outputs

   // UID for each unique instance of this module
   Reg #(Bit #(8)) uid <- mkRegU;

   Reg #(Bool) rg_ip_running <- mkReg (False);

   // Memory request and respons queues
   FIFOF #(BC_Addr)                     f_rd_reqs_e <- mkFIFOF;
   FIFOF #(BC_Data)                     f_rd_rsps_e <- mkFIFOF;
   FIFOF #(Tuple2 #(BC_Addr, BC_Data))  f_wr_reqs_e <- mkFIFOF;

   FIFOF #(BC_Addr)                     f_rd_reqs_o <- mkFIFOF;
   FIFOF #(BC_Data)                     f_rd_rsps_o <- mkFIFOF;
   FIFOF #(Tuple2 #(BC_Addr, BC_Data))  f_wr_reqs_o <- mkFIFOF;

   // ----------------------------------------------------------------
   // State from 'init' method

   // Number of blocks in i, k and j dimensions:
   Reg #(BC_Addr) nb1 <- mkRegU;

   // Block dimensions
   Reg #(Bit#(4)) mm1 <- mkRegU;
   Reg #(Bit#(4)) nn1 <- mkRegU;
   Reg #(Bit#(4)) pp1 <- mkRegU;

   // Address increments
   Reg #(BC_Addr) dAi1 <- mkRegU;
   Reg #(BC_Addr) dAk1 <- mkRegU;
   Reg #(BC_Addr) dAkb <- mkRegU;

   Reg #(BC_Addr) dBk1 <- mkRegU;
   Reg #(BC_Addr) dBj1 <- mkRegU;
   Reg #(BC_Addr) dBkb <- mkRegU;

   Reg #(BC_Addr) dCi1 <- mkRegU;
   Reg #(BC_Addr) dCj1 <- mkRegU;

   // ----------------------------------------------------------------
   // State from 'start' method

   Reg #(BC_Addr) pA <- mkRegU;
   Reg #(BC_Addr) pB <- mkRegU;
   Reg #(BC_Addr) pC <- mkRegU;

   // ----------------------------------------------------------------
   // Module that performs that blocked-MAC accumulation

   BlockMAC_IFC blockMAC <- mkBlockMAC;

   // ----------------------------------------------------------------
   // Generator of read-requests for A blocks
   // nb(k) blocks of i1(mm) x k1(nn) elements

   module mkFSM_reqs_A (FSM);
      Reg #(Bit #(2)) pc   <- mkReg (0);
      Reg #(BC_Addr)  k    <- mkRegU;
      Reg #(Bit #(4)) i1   <- mkRegU;
      Reg #(Bit #(4)) k1   <- mkRegU;
      Reg #(BC_Addr)  pAkb <- mkRegU;
      Reg #(BC_Addr)  pAi1 <- mkRegU;
      Reg #(BC_Addr)  pAk1 <- mkRegU;

      rule rl_init (pc == 1);
	 k1 <= 0;    pAk1 <= pA;
	 i1 <= 0;    pAi1 <= pA;
	 k  <= 0;    pAkb <= pA;
	 pc <= 2;
      endrule

      rule rl_reqs_A (pc == 2);
	 f_rd_reqs_e.enq (pAk1);

	 if (verbosity > 1)
	    $display ("%0d: IP_%2h: req A[%0d][%0d,%0d]: ", cur_cycle, uid, k, i1, k1,
		      fshow (pAk1));

	 if (k1 != nn1) begin
	    k1 <= k1 + 1;
	    pAk1 <= pAk1 + dAk1;
	 end
	 else if (i1 != mm1) begin
	    k1 <= 0;
	    i1 <= i1 + 1;
	    let next_pAi1 = pAi1 + dAi1;
	    pAk1 <= next_pAi1;
	    pAi1 <= next_pAi1;
	 end
	 else if (k != nb1) begin
	    k1 <= 0;
	    i1 <= 0;
	    k  <= k + 1;
	    let next_pAkb = pAkb + dAkb;
	    pAk1 <= next_pAkb;
	    pAi1 <= next_pAkb;
	    pAkb <= next_pAkb;
	 end
	 else
	    pc <= 0;
      endrule

      method Action start ();
	 pc <= 1;
      endmethod

      method Bool done = (pc == 0);
   endmodule: mkFSM_reqs_A

   FSM fsm_reqs_A <- mkFSM_reqs_A;

   // ----------------------------------------------------------------
   // Generator of read-requests for B blocks
   // nb(k) blocks of k1(nn) x j1(pp) elements

   module mkFSM_reqs_B (FSM);
      Reg #(Bit #(2)) pc <- mkReg (0);
      Reg #(BC_Addr)  k    <- mkRegU;
      Reg #(Bit #(4)) j1   <- mkRegU;
      Reg #(Bit #(4)) k1   <- mkRegU;
      Reg #(BC_Addr)  pBkb <- mkRegU;
      Reg #(BC_Addr)  pBk1 <- mkRegU;
      Reg #(BC_Addr)  pBj1 <- mkRegU;

      rule rl_init (pc == 1);
	 j1 <= 0;    pBj1 <= pB;
	 k1 <= 0;    pBk1 <= pB;
	 k  <= 0;    pBkb <= pB;
	 pc <= 2;
      endrule

      rule rl_reqs_B (pc == 2);
	 f_rd_reqs_o.enq (pBj1);

	 if (verbosity > 1)
	    $display ("%0d: IP_%2h: req B[%0d][%0d,%0d]: ", cur_cycle, uid, k, k1, j1,
		      fshow (pBj1));

	 if (j1 != pp1) begin
	    j1 <= j1 + 1;
	    pBj1 <= pBj1 + dBj1;
	 end
	 else if (k1 != nn1) begin
	    j1 <= 0;
	    k1 <= k1 + 1;
	    let next_pBk1 = pBk1 + dBk1;
	    pBj1 <= next_pBk1;
	    pBk1 <= next_pBk1;
	 end
	 else if (k != nb1) begin
	    j1 <= 0;
	    k1 <= 0;
	    k  <= k + 1;
	    let next_pBkb = pBkb + dBkb;
	    pBj1 <= next_pBkb;
	    pBk1 <= next_pBkb;
	    pBkb <= next_pBkb;
	 end
	 else
	    pc <= 0;
      endrule

      method Action start ();
	 pc <= 1;
      endmethod

      method Bool done = (pc == 0);
   endmodule: mkFSM_reqs_B

   FSM fsm_reqs_B <- mkFSM_reqs_B;

   // ----------------
   // Receive A block-row, send to MAC
   // nb(k) blocks x i1(mm) x k1(nn)

   module mkFSM_resps_A (FSM);
      Reg #(Bool)     running <- mkReg (False);
      Reg #(BC_Addr)  k  <- mkRegU;
      Reg #(Bit #(4)) i1 <- mkRegU;
      Reg #(Bit #(4)) k1 <- mkRegU;
      Reg #(Bool)     last_i1_k1 <- mkRegU;

      rule rl_resps_A (running);
	 let aik = f_rd_rsps_e.first; f_rd_rsps_e.deq;
	 let tmp_i1 = i1;
	 let tmp_k1 = k1;

	 blockMAC.putA (tmp_i1, tmp_k1, aik, last_i1_k1);

	 if (tmp_k1 != nn1)
	    tmp_k1 = tmp_k1 + 1;
	 else if (tmp_i1 != mm1) begin
	    tmp_k1 = 0;
	    tmp_i1 = tmp_i1 + 1;
	 end
	 else if (k != nb1) begin
	    tmp_k1 = 0;
	    tmp_i1 = 0;
	    k <= k + 1;
	 end
	 else
	    running <= False;

	 i1   <= tmp_i1;
	 k1   <= tmp_k1;
	 last_i1_k1 <= ((tmp_i1 == mm1) && (tmp_k1 == nn1));

	 if (verbosity > 1)
	    $display ("%0d: IP_%2h: resp A[%0d][%0d,%0d]: %0d", cur_cycle, uid, k, i1, k1, aik);
      endrule

      method Action start ();
	 k  <= 0;
	 i1 <= 0;
	 k1 <= 0;
	 last_i1_k1 <= ((0 == mm1) && (0 == nn1));
	 running <= True;
      endmethod

      method Bool done = (! running);
   endmodule: mkFSM_resps_A

   FSM fsm_resps_A <- mkFSM_resps_A;

   // ----------------
   // Receive B block-row, send to MAC
   // nb(k) blocks x k1(nn) x j1(pp)

   module mkFSM_resps_B (FSM);
      Reg #(Bool)     running <- mkReg (False);
      Reg #(BC_Addr)  k  <- mkRegU;
      Reg #(Bit #(4)) k1 <- mkRegU;
      Reg #(Bit #(4)) j1 <- mkRegU;
      Reg #(Bool)     last_k1_j1 <- mkRegU;

      rule rl_resps_B (running);
	 let bkj = f_rd_rsps_o.first; f_rd_rsps_o.deq;
	 let tmp_k1 = k1;
	 let tmp_j1 = j1;

	 blockMAC.putB (tmp_k1, tmp_j1, bkj, last_k1_j1);

	 if (tmp_j1 != pp1)
	    tmp_j1 = tmp_j1 + 1;
	 else if (tmp_k1 != nn1) begin
	    tmp_j1 = 0;
	    tmp_k1 = tmp_k1 + 1;
	 end
	 else if (k != nb1) begin
	    tmp_j1 = 0;
	    tmp_k1 = 0;
	    k  <= k + 1;
	 end
	 else
	    running <= False;

	 k1 <= tmp_k1;
	 j1 <= tmp_j1;
	 last_k1_j1 <= ((tmp_k1 == nn1) && (tmp_j1 == pp1));

	 if (verbosity > 1)
	    $display ("%0d: IP_%2h: resp B[%0d][%0d,%0d]: %0d", cur_cycle, uid, k, k1, j1, bkj);
      endrule

      method Action start ();
	 j1 <= 0;
	 k1 <= 0;
	 k  <= 0;
	 last_k1_j1 <= ((0 == nn1) && (0 == pp1));
	 running <= True;
      endmethod

      method Bool done = (! running);
   endmodule: mkFSM_resps_B

   FSM fsm_resps_B <- mkFSM_resps_B;

   // ----------------
   // Write a C block data back to memory
   // mm(i1) x nn(j1) elements

   module mkFSM_reqs_C (FSM);
      Reg #(Bit #(2)) pc   <- mkReg (0);
      Reg #(Bit #(4)) i1   <- mkRegU;
      Reg #(Bit #(4)) j1   <- mkRegU;
      Reg #(BC_Addr)  pCi1 <- mkRegU;
      Reg #(BC_Addr)  pCj1 <- mkRegU;
      Reg #(Bool)     last_i1_j1 <- mkRegU;

      // This can't be folded into 'start' method since pC is not ready yet
      rule rl_init (pc == 1);
	 i1 <= 0;    pCi1 <= pC;
	 j1 <= 0;    pCj1 <= pC;
	 last_i1_j1 <= ((0 == mm1) && (0 == pp1));
	 pc <= 2;
      endrule

      rule rl_C_block_writer (pc == 2);
	 let tmp_i1 = i1;
	 let tmp_j1 = j1;

	 let d <- blockMAC.getC (tmp_i1, tmp_j1, last_i1_j1);
	 let wr_req  = tuple2 (pCj1, d);
	 let f_wr_reqs = ((pCj1 [3] == 1'b0) ? f_wr_reqs_e : f_wr_reqs_o);
	 f_wr_reqs.enq (wr_req);

	 if (tmp_j1 != pp1) begin
	    tmp_j1 = tmp_j1 + 1;
	    pCj1 <= pCj1 + dCj1;
	 end
	 else if (tmp_i1 != mm1) begin
	    tmp_j1 = 0;
	    tmp_i1 = tmp_i1 + 1;
	    let next_pCi1 = pCi1 + dCi1;
	    pCi1 <= next_pCi1;
	    pCj1 <= next_pCi1;
	 end
	 else
	    pc <= 0;

	 i1 <= tmp_i1;
	 j1 <= tmp_j1;
	 last_i1_j1 <= ((tmp_i1 == mm1) && (tmp_j1 == pp1));

	 if (verbosity > 1)
	    $display ("%0d: IP_%2h: write req C[%0d,%0d]: %0d", cur_cycle, uid, tmp_i1, tmp_j1,
		      fshow (wr_req));
      endrule

      method Action start ();
	 pc <= 1;
      endmethod

      method Bool done = (pc == 0);
   endmodule: mkFSM_reqs_C

   FSM fsm_reqs_C <- mkFSM_reqs_C;

   // ----------------------------------------------------------------
   // Run all FSMs

   rule rl_completion (rg_ip_running &&
		       fsm_reqs_A.done && fsm_resps_A.done &&
		       fsm_reqs_B.done && fsm_resps_B.done &&
		       fsm_reqs_C.done);
      rg_ip_running <= False;
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   function Client #(BC_Addr, BC_Data) toClient (FIFOF #(BC_Addr) f_rd_reqs,
						 FIFOF #(BC_Data) f_rd_rsps);
      return interface Client;
		interface Get request = toGet (f_rd_reqs);
		interface Put response = toPut (f_rd_rsps);
	     endinterface;
   endfunction

   method Action init (Bit #(8) uid_arg,
		       Bit#(4) mm1_arg, Bit#(4) nn1_arg, Bit#(4) pp1_arg,
		       BC_Addr nb1_arg,
		       BC_Addr dAi1_arg, BC_Addr dAk1_arg, BC_Addr dAkb_arg,
		       BC_Addr dBk1_arg, BC_Addr dBj1_arg, BC_Addr dBkb_arg,
		       BC_Addr dCi1_arg, BC_Addr dCj1_arg);

      uid <= uid_arg;
      mm1 <= mm1_arg;  nn1 <= nn1_arg;  pp1 <= pp1_arg;
      nb1 <= nb1_arg;
      dAi1 <= dAi1_arg; dAk1 <= dAk1_arg; dAkb <= dAkb_arg;
      dBk1 <= dBk1_arg; dBj1 <= dBj1_arg; dBkb <= dBkb_arg;
      dCi1 <= dCi1_arg; dCj1 <= dCj1_arg;

      blockMAC.init (uid_arg, mm1_arg, nn1_arg, pp1_arg, nb1_arg);
   endmethod

   // Start a block-inner-product
   method Action start (BC_Addr pA_arg, BC_Addr pB_arg, BC_Addr pC_arg) if (! rg_ip_running);
      pA <= pA_arg; pB <= pB_arg; pC <= pC_arg;
      fsm_reqs_A.start;    fsm_resps_A.start;
      fsm_reqs_B.start;    fsm_resps_B.start;
      fsm_reqs_C.start;
      rg_ip_running <= True;
   endmethod

   method Bool running = rg_ip_running;

   interface rd_client_e = toClient (f_rd_reqs_e, f_rd_rsps_e);
   interface wr_client_e = toGet (f_wr_reqs_e);
   interface rd_client_o = toClient (f_rd_reqs_o, f_rd_rsps_o);
   interface wr_client_o = toGet (f_wr_reqs_o);
endmodule: mkInnerProduct

// ================================================================

endpackage
