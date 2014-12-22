// Copyright (c) 2012-2015 Bluespec, Inc.  All Rights Reserved
// Distributed under license.

// Author: Rishiyur S. Nikhil

package SpecialFIFOs2;

// ================================================================
// This package is BSV source-level implementation of
//     mkPipelineFIFOF
//     mkBypassFIFOF
//     mkSizedFIFOF_AlmostFull
// all with optional guarding.

// The first two are normally found in BSV lib SpecialFIFOs.

// The latter is similar to those found in BSV lib FIFOLevel,
//     but it avoids using the imported Verilog lib SizedFIOF.v
//     for which Xilinx XST sometimes produces flaky circuits
//         (flaky = timing sensitive with no warning in synth logs)

// These BSV source versions also allow easy modification by the user,
// if needed.

// ================================================================
// BSV library imports

import FIFOF  :: *;    // for FIFOF interface def
import Vector :: *;

// ================================================================
// This type is used to specify the desired scheduling semantics

// Pipeline schedule:
//     {notEmpty, first} <= deq < notFull <= enq < clear
// Bypass schedule:
//     notFull <= enq < {notEmpty, first} <= deq < clear

typedef enum { SchedPipeline, SchedBypass } FIFOSched
    deriving (Eq);

// ================================================================
// mkSpecialFIFOF is parameterized to choose desired scheduling
// semantics, as well as whether the enq side or deq side or both
// are guarded/unguarded

module mkSpecialFIFOF #(FIFOSched sched, Bool ug_enq, Bool ug_deq)
                      (FIFOF #(t))
   provisos (Bits #(t, tsz));

   Reg #(t)        cr_data[3]  <- mkCRegU (3);
   Reg #(Bool)     cr_empty[3] <- mkCReg (3, True);

   Integer enqport = ((sched == SchedPipeline) ? 1 : 0);
   Integer deqport = ((sched == SchedPipeline) ? 0 : 1);
   Integer clrport = 2;

   // ---- Enqueue side

   method Bool notFull ();
      return cr_empty [enqport];
   endmethod

   method Action enq (t x) if (ug_enq || cr_empty [enqport]);
      cr_data  [enqport] <= x;
      cr_empty [enqport] <= False;
   endmethod

   // ---- Dequeue side

   method Bool notEmpty ();
      return (! cr_empty [deqport]);
   endmethod

   method t first () if (ug_deq || (! cr_empty [deqport]));
      return cr_data [deqport];
   endmethod

   method Action deq () if (ug_deq || (! cr_empty [deqport]));
      cr_empty [deqport] <= True;
   endmethod

   // ---- Clear

   method Action clear ();
      cr_empty [clrport] <= True;
   endmethod

endmodule

// ================================================================
// These define the conventional Pipeline and Bypass FIFOs
// with guarded enq and deq methods
// The '2' suffix is just to distinguish them from the lib versions

module mkPipelineFIFOF2 (FIFOF #(t)) provisos (Bits #(t, tsz));
   let ifc <- mkSpecialFIFOF (SchedPipeline, False, False);
   return ifc;
endmodule

module mkBypassFIFOF2 (FIFOF #(t)) provisos (Bits #(t, tsz));
   let ifc <- mkSpecialFIFOF (SchedBypass, False, False);
   return ifc;
endmodule

// ================================================================
// SizedFIFOF_AlmostFull asserts 'full' when there are
// still 'headroom' slots available.
// First two parameers are only used for their type, not their value.
// Used in flow-control situations where, after asserting flow control,
// a few (bounded) number of items may still arrive.
// Has 'pipelineFIFO' schedule:
//     {notEmpty,first,deq} < {notFull,enq} < clear

module mkSizedFIFOF_AlmostFull #(Bit #(size)      dummy_size,
				 Bit #(headroom)  dummy_headroom,
				 Bool ugenq,
				 Bool ugdeq)
                               (FIFOF #(t))
   provisos (Bits #(t, tsz),
	     Add #(size, headroom, n),
	     Log #(n, logn),
	     Add #(n, 1, n1),
	     Log #(n1, logn1));

   if (valueOf (size) < 1)     errorM ("ERROR: mkSizedFIFOF_AlmostFull: size is < 1");

   Vector #(n, Reg #(t)) vec <- replicateM (mkRegU);    // size+headroom
   Reg #(UInt #(logn1))  cr_nItems[3] <- mkCReg (3, 0);
   Reg #(UInt #(logn))   rg_hd        <- mkReg (0);
   Reg #(UInt #(logn))   rg_tl        <- mkReg (0);
   
   // ----------------

   function UInt #(logn) modIncr (UInt #(logn) j);
      return ((j == fromInteger (valueOf (n) - 1)) ? 0 : j + 1);
   endfunction

   Bool vNotEmpty = (cr_nItems [0] != 0);
   Bool vNotFull  = (cr_nItems [1] < fromInteger (valueOf (size)));

   // ----------------
   // INTERFACE first/dequeue side (uses cr_nItems [0])

   method Bool notEmpty ();
      return vNotEmpty;
   endmethod

   method t  first () if (ugdeq || vNotEmpty);
      return vec [rg_hd];
   endmethod

   method Action deq () if (ugdeq || vNotEmpty);
      cr_nItems [0] <= cr_nItems [0] - 1;
      rg_hd <= modIncr (rg_hd);
   endmethod

   // ----------------
   // INTERFACE enq side (uses cr_nItems [1])

   method Bool notFull ();
      return vNotFull;
   endmethod

   method Action enq (t  x) if (ugenq || vNotFull);
      vec [rg_tl] <= x;
      cr_nItems [1] <= cr_nItems [1] + 1;
      rg_tl <= modIncr (rg_tl);
   endmethod

   // ----------------
   // INTERFACE clear (uses cr_nItems [2])

   method Action clear ();
      cr_nItems [2] <= 0;
      rg_hd <= 0;
      rg_tl <= 0;
   endmethod
endmodule

// ================================================================
// A dummy FIFOF that behaves like it is always full and always empty
// Used to tie-off unused sources and sinks

module mkDummyFIFOF (FIFOF #(t));
   method Action enq (t x) if (False);
      noAction;
   endmethod

   method t first if (False);
      return ?;
   endmethod

   method Action deq if (False);
      noAction;
   endmethod

   method Action clear if (False);
      noAction;
   endmethod

   method Bool notFull  = False;
   method Bool notEmpty = False;
endmodule

// ================================================================

endpackage
