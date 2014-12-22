// Copyright (c) 2013-2015 Bluespec, Inc. All Rights Reserved.
//
// Author: Rishiyur Nikhil, Bluespec, Inc.

package Mult;

// ================================================================
// This package defines a pipelined multiplier for Int#(64) values
// that uses multiple 16-bit unsigned multiplications on 16-bit
// components of the args, followed by shifts and adds, etc.
// The purpose is to alleviate timing closure.

// ================================================================
// Bluespec library imports

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Vector       :: *;

// ================================================================
// The interface for the multiplier module

typedef Server #(Tuple2 #(Int#(64), Int#(64)), Int#(64))
        MultInt64_IFC;

// ================================================================
// The multiplier module

(* synthesize *)
module mkMultInt64 (MultInt64_IFC);

   FIFOF #(Tuple2 #(Int#(64), Int#(64)))           f_in        <- mkFIFOF;
   FIFOF #(Tuple3 #(Bool, Bit#(64), Bit#(64)))     f_u64s      <- mkFIFOF;
   FIFOF #(Tuple2 #(Bool, Vector #(10, Bit#(64)))) f_partial10 <- mkFIFOF;
   FIFOF #(Tuple2 #(Bool, Vector #(5, Bit#(64))))  f_partial5  <- mkFIFOF;
   FIFOF #(Tuple2 #(Bool, Vector #(3, Bit#(64))))  f_partial3  <- mkFIFOF;
   FIFOF #(Tuple2 #(Bool, Vector #(2, Bit#(64))))  f_partial2  <- mkFIFOF;
   FIFOF #(Tuple2 #(Bool, Bit#(64)))               f_partial1  <- mkFIFOF;
   FIFOF #(Int#(64))                                f_out       <- mkFIFOF;

   // This function does a 32-bit multiply on two 16-bit values,
   // and then shifts it into position in a 64-bit result
   function Bit#(64) multsh (Bit#(16) a16, Bit#(16) b16, Nat n);
      Bit#(32) a32 = extend (a16);
      Bit#(32) b32 = extend (b16);
      Bit#(32) c32 = a32 * b32;
      Bit#(64) c64 = extend (c32);
      return (c64 << n);
   endfunction

   // Convert inputs to unsigneds, remember sign of output
   rule rl_in_to_u64s;
      match { .ai64, .bi64 } = f_in.first; f_in.deq;
      Bool neg_a = (ai64 < 0);
      Bool neg_b = (bi64 < 0);
      Bool neg_c = (neg_a != neg_b);

      if (neg_a) ai64 = -ai64;
      if (neg_b) bi64 = -bi64;

      Bit#(64) au64 = pack (ai64);
      Bit#(64) bu64 = pack (bi64);

      f_u64s.enq (tuple3 (neg_c, au64, bu64));
   endrule

   // Do 16-bit multiplies
   rule rl_partial_prods;
      match { .neg_c, .au64, .bu64 } = f_u64s.first; f_u64s.deq;
      Bit#(16) a3 = au64[63:48];      Bit#(16) b3 = bu64[63:48];
      Bit#(16) a2 = au64[47:32];      Bit#(16) b2 = bu64[47:32];
      Bit#(16) a1 = au64[31:16];      Bit#(16) b1 = bu64[31:16];
      Bit#(16) a0 = au64[15: 0];      Bit#(16) b0 = bu64[15: 0];

      Vector #(10, Bit#(64)) v10 = newVector;

      v10[0] = multsh(a3,b0,48);  v10[1] = multsh(a2,b1,48);
      v10[2] = multsh(a1,b2,48);  v10[3] = multsh(a0,b3,48);

      v10[4] = multsh(a2,b0,32);  v10[5] = multsh(a1,b1,32);
      v10[6] = multsh(a0,b2,32);

      v10[7] = multsh(a1,b0,16);  v10[8] = multsh(a0,b1,16);

      v10[9] = multsh(a0,b0,0);
      f_partial10.enq (tuple2 (neg_c, v10));
   endrule

   // Partial sum
   rule rl_sum_10_to_5;
      match { .neg_c, .v10 } = f_partial10.first; f_partial10.deq;

      Vector #(5, Bit#(64)) v5 = newVector;
      for (Integer j = 0; j < 5; j = j + 1)
	 v5[j] = v10[2*j] + v10[2*j+1];

      f_partial5.enq (tuple2 (neg_c, v5));
   endrule

   // Partial sum
   rule rl_sum_5_to_3;
      match { .neg_c, .v5 } = f_partial5.first; f_partial5.deq;

      Vector #(3, Bit#(64)) v3 = newVector;
      v3[0] = v5[0] + v5[1];
      v3[1] = v5[2] + v5[3];
      v3[2] = v5[4];

      f_partial3.enq (tuple2 (neg_c, v3));
   endrule

   // Partial sum
   rule rl_sum_3_to_2;
      match { .neg_c, .v3 } = f_partial3.first; f_partial3.deq;

      Vector #(2, Bit#(64)) v2 = newVector;
      v2[0] = v3[0] + v3[1];
      v2[1] = v3[2];

      f_partial2.enq (tuple2 (neg_c, v2));
   endrule

   // Partial sum
   rule rl_sum_2_to_1;
      match { .neg_c, .v2 } = f_partial2.first; f_partial2.deq;

      Bit #(64) y = v2[0] + v2[1];

      f_partial1.enq (tuple2 (neg_c, y));
   endrule

   // Convert back to signed int, send to output
   rule rl_u64_to_i64;
      match { .neg_c, .u64 } = f_partial1.first; f_partial1.deq;

      Int#(64) i64 = unpack (u64);
      if (neg_c) i64 = - i64;
      f_out.enq (i64);
   endrule

   // ----------------
   // INTERFACE

   interface request  = toPut (f_in);
   interface response = toGet (f_out);

endmodule: mkMultInt64

// ================================================================

endpackage
