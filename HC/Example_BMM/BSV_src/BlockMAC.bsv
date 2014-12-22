// Copyright (c) 2013-2015 Bluespec, Inc. All Rights Reserved.
//
// Author: Rishiyur Nikhil, Bluespec, Inc.

package BlockMAC;

// ================================================================
// Bluespec library imports

import RegFile      :: *;
import FIFOF        :: *;
import GetPut       :: *;

// ----------------
// BC library imports

import BC_Utils           :: *;
import BC_HW_IFC          :: *;

// ----------------
// Local imports

import ScalarMAC :: *;

// ================================================================
// This module performs the MAC at the level of blocks:
//     Initialize a c_block to zero
//     Loop k = mm x pp times:
//         receive mm x nn data for a_block
//         and     nn x pp data for b_block
//         c_block <= mat_add (c_block, mat_mult (a_block, b_block))
//     Yield the final c_block

interface BlockMAC_IFC;
   method Action init (Bit #(8) uid_arg,
		       Bit #(4) mm1_arg, Bit #(4) nn1_arg, Bit #(4) pp1_arg,
		       BC_Addr nb1_arg);
   method Action putA (Bit #(4) i1, Bit #(4) k1, BC_Data a_val, Bool last);
   method Action putB (Bit #(4) k1, Bit #(4) j1, BC_Data b_val, Bool last);
   method ActionValue #(BC_Data) getC (Bit #(4) i1, Bit #(4) j1, Bool last);
endinterface

(* synthesize *)
module mkBlockMAC (BlockMAC_IFC);

   Integer verbosity = 0;

   Reg #(Bit #(8)) uid <- mkRegU;

   Reg #(Bit #(3)) pc  <- mkReg (0);
   Reg #(Bit #(3)) pc1 <- mkReg (0);

   Reg #(BC_Addr)  nb1 <- mkRegU;
   Reg #(Bit #(4)) mm1 <- mkRegU;
   Reg #(Bit #(4)) nn1 <- mkRegU;
   Reg #(Bit #(4)) pp1 <- mkRegU;

   Reg #(BC_Addr)  k  <- mkRegU;
   Reg #(Bit #(4)) i1 <- mkRegU;
   Reg #(Bit #(4)) j1 <- mkRegU;
   Reg #(Bit #(4)) k1 <- mkRegU;

   ScalarMAC_IFC scalarMAC <- mkScalarMAC_Int64;

   // ----------------
   // Buffers holding a block of a, b and c
   // TODO: max block size is 16 elements, e.g., 4x4. Is this ok?
   RegFile #(Bit #(4), BC_Data) a_block <- mkRegFileFull;
   RegFile #(Bit #(4), BC_Data) b_block <- mkRegFileFull;
   RegFile #(Bit #(4), BC_Data) c_block <- mkRegFileFull;

   function Bit#(4) index (Bit#(4) x, Bit#(4) y, Bit#(4) yDim);
      Bit #(4) x4 = x[3:0];
      Bit #(4) y4 = y[3:0];
      // Compute x * yDim + y, assuming yDim is 0, 1 or 3
      Bit #(4) x_yDim = ((yDim == 0) ?  x4
			 : ((yDim == 1) ? (x4 << 1)
			    : ((yDim == 3) ? (x4 << 2)
			       : ?)));    // Impossible
      return (x_yDim | y4);
   endfunction

   function Bit#(4) a_index (Bit #(4) i1, Bit #(4) k1) = index (i1,k1,nn1);
   function Bit#(4) b_index (Bit #(4) k1, Bit #(4) j1) = index (k1,j1,pp1);
   function Bit#(4) c_index (Bit #(4) i1, Bit #(4) j1) = index (i1,j1,pp1);

   // ----------------
   // Clear the C block at start of inner product

   rule rl_init_c_block (pc == 1);
      c_block.upd (c_index (i1, j1), 0);
      if (j1 != pp1)
	 j1 <= j1 + 1;
      else if (i1 != mm1) begin
	 j1 <= 0;
	 i1 <= i1 + 1;
      end
      else begin
	 k   <= 0;
	 i1  <= 0;
	 j1  <= 0;
	 pc  <= 2;
	 pc1 <= 2;
      end
   endrule

   // ----------------
   // Wait for putA, putB and init_c_block to complete

   rule rl_join ((pc == 3) && (pc1 == 3));
      pc <= 4;
      i1 <= 0;
      j1 <= 0;

      if (verbosity > 1)
	 $display ("%0d: blockMAC_%2h: join: a and b blocks received", cur_cycle, uid);
   endrule

   // ----------------
   // The MAC op

   rule rl_MAC_i1_j1_loop (pc == 4);
      let c = c_block.sub (c_index (i1, j1));
      scalarMAC.put_c.put (c);
      k1 <= 0;
      pc <= 5;
   endrule

   rule rl_MAC_k1_loop (pc == 5);
      let a = a_block.sub (a_index (i1, k1));
      let b = b_block.sub (b_index (k1, j1));
      Bool last = (k1 == nn1);

      scalarMAC.put_a_b_last.put (tuple3 (a, b, last));

      if (! last)
	 k1 <= k1 + 1;
      else
	 pc <= 6;

      if (verbosity > 1) begin
	 $display ("%0d: blockMAC_%2h: join: a and b blocks received", cur_cycle, uid);
	 $display ("%0d: blockMAC_%2h: a[%0d,%0d] b[%0d,%0d]", cur_cycle, uid, i1, k1, k1, j1);
      end
   endrule

   rule rl_MAC_i1_j2_loop_step2 (pc == 6);
      let c <- scalarMAC.get_c.get;
      c_block.upd (c_index (i1, j1), c);
      if (j1 != pp1) begin
	 j1 <= j1 + 1;
	 pc <= 4;    // top of i1,j1 loop
      end
      else if (i1 != mm1) begin
	 j1 <= 0;
	 i1 <= i1 + 1;
	 pc <= 4;    // top of i1,j1 loop
      end
      else if (k != nb1) begin    // Receive next block of A and B
	 k <= k + 1;
	 pc <= 2;
	 pc1 <= 2;
      end
      else
	 pc <= 7;    // MAC done; enable getC
   endrule

   // ----------------
   // INTERFACE

   method Action init (Bit #(8) uid_arg,
		       Bit #(4) mm1_arg, Bit #(4) nn1_arg, Bit #(4) pp1_arg, BC_Addr nb1_arg);
      uid <= uid_arg;
      mm1 <= mm1_arg;
      nn1 <= nn1_arg;
      pp1 <= pp1_arg;
      nb1 <= nb1_arg;

      pc <= 1;  i1 <= 0;  j1 <= 0;    // enable c_block reset to 0
   endmethod

   method Action putA (Bit #(4) i1, Bit #(4) k1, BC_Data a_val, Bool last) if (pc == 2);
      a_block.upd (a_index (i1, k1), a_val);
      if (last) pc <= 3;
   endmethod

   method Action putB (Bit #(4) k1, Bit #(4) j1, BC_Data b_val, Bool last) if (pc1 == 2);
      b_block.upd (b_index (k1, j1), b_val);
      if (last) pc1 <= 3;
   endmethod

   method ActionValue #(BC_Data) getC (Bit #(4) i1, Bit #(4) j1, Bool last) if (pc == 7);
      if (last)    // reset pc for next inner-product
	 pc  <= 1;
      return c_block.sub (c_index (i1, j1));
   endmethod
endmodule: mkBlockMAC

// ================================================================

endpackage
