// Copyright (c) 2012-2015, Bluespec, Inc., All Rights Reserved
// Distributed under license.

// Author: Rishiyur S. Nikhil

package RingNet;

// ================================================================

// This is an m-in, n-out routing network, implemented as a Ring.
// I.e., there is an internal ring shift register (last element feeds
// back into the first element) of slots carrying packets.  Its size
// is max(m,n). Each of the m inputs feeds packets into one of the
// slots, and each of the n outputs extracts packets destined for that
// output from one of the slots. Usual BSV FIFO-like flow control.

// Polymorphic in m, n, packet type, and a "destination" function (packet,n)->Bool

// ================================================================

import Vector        :: *;
import FIFOF         :: *;
import GetPut        :: *;
import FShow         :: *;
import SpecialFIFOs2 :: *;

export RingNet_IFC (..), mkRingNet;

// ================================================================

interface RingNet_IFC #(numeric type m, numeric type n, type t_pkt);
   interface Vector #(m, Put #(t_pkt)) inputs;
   interface Vector #(n, Get #(t_pkt)) outputs;

   method Action setDrainState (Bool state);
   method Bool drained;
endinterface

// ================================================================

module mkRingNet
   #(function Bool destination (t_pkt pkt,Bit #(log_n) output_number))
   (RingNet_IFC #(m, n, t_pkt))
   provisos (FShow #(t_pkt),
	     Bits #(t_pkt, t_pkt_sz),
	     Log #(n, log_n),
	     Max #(m, n, mn),
	     Log #(mn, log_mn),
	     Add #(c_, log_n, log_mn),
	     Add #(m, a_, mn),     // Compiler ought to know this
	     Add #(n, b_, mn));    // Compiler ought to know this

   // ----------------------------------------------------------------
   // STATE

   Reg #(Bool) rg_initialized <- mkReg (False);

   Vector #(mn, FIFOF #(t_pkt)) vf_ins;
   Vector #(mn, FIFOF #(t_pkt)) vf_outs;

   // Ring buffer
   Vector #(mn, Reg #(Maybe #(t_pkt))) vrg_buf <- replicateM (mkRegU);

   Bool ugenq = True;
   Bool ugdeq = True;

   for (Integer j = 0; j < valueOf (mn); j = j + 1) begin
      vf_ins  [j] <- (  (j < valueOf (m))
		      ? mkSpecialFIFOF (SchedPipeline, (! ugenq), ugdeq)
		      : mkDummyFIFOF);

      vf_outs [j] <- (  (j < valueOf (n))
		      ? mkSpecialFIFOF (SchedPipeline, ugenq, (! ugdeq))
		      : mkDummyFIFOF);
   end

   Reg #(Bool) rg_drain <- mkReg (False);
   Reg #(Bool) all_input_fifos_empty  <- mkReg(False);
   Reg #(Bool) ring_buffer_is_empty   <- mkReg(False);
   Reg #(Bool) all_output_fifos_empty <- mkReg(False);
	
   // ----------------------------------------------------------------

   function Bit #(log_mn) adjust_width (Bit #(log_n) x) = extend (x);

   // ----------------------------------------------------------------
   // BEHAVIOR

   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_initialize (! rg_initialized);
      for (Integer j = 0; j < valueOf (mn); j = j + 1)
	 vrg_buf [j] <= tagged Invalid;
      rg_initialized <= True;
   endrule

   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_doit (rg_initialized);
      /* // ---- for debugging
      $write ("    ringbuf: ");
      for (Integer j = 0; j < valueOf (mn); j = j + 1) begin
	 $write (" ", fshow (vrg_buf [j]));
      end
      $display ("");
      */
      for (Integer j = 0; j < valueOf (mn); j = j + 1) begin
	 let jprev = ( (j == 0) ? (valueOf (mn) - 1) : (j - 1));
	 let mpkt = vrg_buf [jprev];
	 if (jprev < valueOf (n)) begin
	    Bit #(log_mn) j1 = fromInteger (jprev);
	    if (     mpkt matches tagged Valid .pkt
	       &&&  destination (pkt, truncate (j1))
	       &&&  vf_outs [jprev].notFull)
	       begin
		  vf_outs [jprev].enq (pkt);
		  mpkt = tagged Invalid;
	       end
	 end
	 if (mpkt matches tagged Invalid  &&&  vf_ins [j].notEmpty) begin
	    let pkt = vf_ins [j].first; vf_ins [j].deq;
	    mpkt = tagged Valid pkt;
	 end
	 vrg_buf [j] <= mpkt;
      end
   endrule
   
   function Bool is_fifo_not_empty(FIFOF#(t) f);
      return f.notEmpty;
   endfunction
      
   (* fire_when_enabled, no_implicit_conditions *)
   rule monitor_drain_status;
      Vector#(m,Bool)  ins_not_empty        = map(is_fifo_not_empty, take(vf_ins));
      Vector#(mn,Bool) ring_slots_not_empty = map(isValid, readVReg(vrg_buf));
      Vector#(n,Bool)  outs_not_empty       = map(is_fifo_not_empty, take(vf_outs));
      all_input_fifos_empty  <= !\or (ins_not_empty);
      ring_buffer_is_empty   <= !\or (ring_slots_not_empty);
      all_output_fifos_empty <= !\or (outs_not_empty);
   endrule
   
   // ----------------------------------------------------------------
   // INTERFACE
   
   function Put#(t) put_when_not_draining(FIFOF#(t) f);
      return (interface Put#(t);
		 method Action put(t x) if (!rg_drain);
		    f.enq(x);
		 endmethod
  	      endinterface);
   endfunction
      
   interface inputs  = map (put_when_not_draining, take (vf_ins));      
   interface outputs = map (toGet, take (vf_outs));

   method Action setDrainState (Bool state);
      rg_drain <= state;
   endmethod
   
   method Bool drained =  all_input_fifos_empty
                       && ring_buffer_is_empty
                       && all_output_fifos_empty
                       ;
endmodule

// ================================================================

endpackage
