// Copyright (c) 2012-2015 Bluespec, Inc., All Rights Reserved
// Distributed under license.

// Author: Rishiyur S. Nikhil

package CompletionBuffer2;

// ================================================================
// This is a replacement for the BSV library Completion Buffer.
// In the library version:
//     'reserve' and 'complete' cannot be in same Action
//     'reserve' and 'complete' conflict (can't fire in same cycle)
// and so it cannot behave like a pipelined FIFO

// This version fixes those scheduling problems

// ================================================================

import Vector           :: *;
import GetPut           :: *;
import FIFOF            :: *;
import BC_Utils         :: *;

export CBToken2, CompletionBuffer2(..), mkCompletionBuffer2;

// ================================================================
// The following is to create CBToken2 as an abstract type,
// by not exporting the ix field

typedef struct {
   UInt #(TLog #(n))  ix;
   } CBToken2 #(numeric type n)
deriving (Bits);

// ----------------------------------------------------------------

interface CompletionBuffer2 #(numeric type n, type t);
   interface Get #(CBToken2 #(n))               reserve;
   interface Put #(Tuple2 #(CBToken2 #(n), t))  complete;
   interface Get #(t)                           drain;
endinterface

// ----------------------------------------------------------------

module mkCompletionBuffer2 (CompletionBuffer2 #(n, t))
   provisos (Bits #(t, tsz),
	     Log #(n, logn));

   // The following FIFOs are just used to register inputs and outputs
   // before they fan-out to/fan-in from the vr_data array
   FIFOF #(CBToken2 #(n))               f_tokens  <- mkFIFOF;
   FIFOF #(Tuple2 #(CBToken2 #(n), t))  f_inputs  <- mkFIFOF;
   FIFOF #(t)                           f_outputs <- mkFIFOF;

   // This is the reorder buffer
   Vector #(n, Reg #(Maybe #(t))) vr_data <- replicateM (mkInvalidReg);

   // Metadata for the reorder buffer
   Reg #(Tuple3 #(UInt #(logn),
		  UInt #(logn),
		  Bool))         cr_head_next_full [3] <- mkCReg (3, tuple3 (0,0, False));

   match { .head0, .next0, .full0 } = cr_head_next_full [0];
   match { .head1, .next1, .full1 } = cr_head_next_full [1];

   function UInt #(logn) modulo_incr (UInt #(logn) j);
      return ( (j == fromInteger (valueOf (n) - 1)) ? 0 : j+1);
   endfunction

   // do a 'deq' (creg [0])
   rule rl_move_outputs (vr_data [head0] matches tagged Valid .v  &&& ((head0 != next0) || full0));
      vr_data [head0] <= tagged Invalid;
      cr_head_next_full [0] <= tuple3 (modulo_incr (head0), next0, False);
      f_outputs.enq (v);
   endrule

   // reserve an 'enq' slot at the current tail (creg [1])
   rule rl_move_tokens (! full1);
      let next_next1 = modulo_incr (next1);
      cr_head_next_full [1] <= tuple3 (head1, next_next1, (head1==next_next1));
      f_tokens.enq (CBToken2 { ix: next1 });
   endrule

   // do the 'enq' (current tail may be beyond this point)
   rule rl_move_inputs;
      match { .tok, .x } = f_inputs.first; f_inputs.deq;
      vr_data [tok.ix] <= tagged Valid x;
   endrule

   // ----------------------------------------------------------------
   // Interface

   interface reserve  = toGet (f_tokens);
   interface complete = toPut (f_inputs);
   interface drain    = toGet (f_outputs);
endmodule

// ================================================================

endpackage
