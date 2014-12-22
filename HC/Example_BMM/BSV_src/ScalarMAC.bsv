// Copyright (c) 2013-2015 Bluespec, Inc. All Rights Reserved.
//
// Author: Rishiyur Nikhil, Bluespec, Inc.

package ScalarMAC;

// ================================================================
// This module implements the core 64-bit pipelined scalar MAC.
// Here we're interpreting BC_Data as Int#(64);
// it can be replaced by floating point, for example

// ================================================================
// Bluespec library imports

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

// ----------------
// BC library imports

import BC_Utils           :: *;
import BC_HW_IFC          :: *;

// ----------------
// Local imports

import Mult :: *;

// ================================================================

interface ScalarMAC_IFC;
   method Action init_c (Bit #(64) c);
   interface Put #(BC_Data) put_c;
   interface Put #(Tuple3 #(BC_Data, BC_Data, Bool)) put_a_b_last;
   interface Get #(BC_Data) get_c;
endinterface

(* synthesize *)
module mkScalarMAC_Int64 (ScalarMAC_IFC);

   FIFOF #(BC_Data)                          f_c_in     <- mkFIFOF;
   FIFOF #(Tuple3 #(BC_Data, BC_Data, Bool)) f_a_b_last <- mkFIFOF;
   MultInt64_IFC                             multInt64  <- mkMultInt64;
   FIFOF #(Bool)                             f_withMult <- mkSizedFIFOF (16);
   FIFOF #(BC_Data)                          f_c_out    <- mkFIFOF;

   Reg #(Bool)      rg_c_valid <- mkReg (False);
   Reg #(Int #(64)) rg_c       <- mkRegU;


   // ----------------
   // BEHAVIOR

   rule rl_c_in (! rg_c_valid);
      rg_c_valid <= True;
      rg_c <= unpack (f_c_in.first); f_c_in.deq;
   endrule

   rule rl_a_x_b;
      match { .da, .db, .last } = f_a_b_last.first;  f_a_b_last.deq;
      Int #(64) a = unpack (da);
      Int #(64) b = unpack (db);
      multInt64.request.put (tuple2 (a,b));
      f_withMult.enq (last);
   endrule

   rule rl_c_plus_ab (rg_c_valid);
      let ab <- multInt64.response.get;
      let last = f_withMult.first; f_withMult.deq;

      let c = rg_c + ab;
      rg_c <= c;
      if (last) begin
	 f_c_out.enq (pack (c));
	 rg_c_valid <= False;
      end
   endrule

   // ----------------
   // INTERFACE

   method Action init_c (Bit #(64) c);
      rg_c <= unpack (c);
   endmethod

   interface put_c        = toPut (f_c_in);
   interface put_a_b_last = toPut (f_a_b_last);
   interface get_c        = toGet (f_c_out);

endmodule: mkScalarMAC_Int64

// ================================================================

endpackage
