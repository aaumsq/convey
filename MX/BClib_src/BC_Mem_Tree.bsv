// Copyright (c) 2012-2015 Bluespec, Inc.  All Rights Reserved
// Distributed under license.

// Author: Rishiyur S. Nikhil

package BC_Mem_Tree;

// ================================================================
// This package implements a module mkMemTreeNode that builds
// a node in a "self-routing fat tree":
//  - transports requests up from leaves to the root, and
//  - transports responses down from the root to the leaves
//
// Each node is parameterized by m (# of leaves) and n (# of upward links)
// If n=1 we get a conventional tree node (1 parent, m children),
//                which has an m:1 reduction in bandwidth going up.
// If n=m we get a "full bandwdith" fat tree (m parents, m children),
//                which has no reduction in bandwidth going up.
// For n = 2..(m-1) we get n/m reduction in bandwidth.
// (n > m is functionally ok, but might be a waste of resources.)
//
// Self-routing:
//   Requests moving up the tree are routed on any available upward port.
//
//   The Bit#(32) 'rtnctl' value that accompanies a request upward acts as a
//   "stack" on which we "push" the leaf-side port number on which we entered.
//   Specifically, we shift log(m) bits, the port number, into the LSBs,
//   i.e., we shift it left into the Bit#(32) value.
//
//   The Bit#(32) 'rtnctl' value that accompanies a response downward specifies
//   the leaf-side port number to which it will be routed.  On exit, we shift
//   the Bit #(32) value right by log(m) bits, discarding log(m) bits.
//   I.e., we "pop" those routing bits off the Bit #(32) "stack".
//
// Fat trees are constructed by composing such nodes, and the tree
// typically mirrors the module hierarchy.  Since nodes are constructed
// individually, the tree does not have to be uniform, symmetric, or balanced,
// i.e., each node can have a different m and n, and the depth of the tree
// may be different on different paths.  The "stack-based" routing described
// above will work automatically as you compose a tree from nodes.
//
// At the top of the tree, at each of the upper n ports, a server receives
// a request and a Bit #(32) value representing the "return address".
// The server should return the response with the same Bit #(32) value,
// which will be used to by the nodes to route the response back to the
// request originator leaf.
//
// Note: the stack is 32b deep, which should be deep enough for most
// practical applications.  There is no check for stack overflows or
// underflows.

// ================================================================
// BSV libs

import Vector        :: *;
import FIFOF         :: *;
import GetPut        :: *;
import ClientServer  :: *;
import Connectable   :: *;
import SpecialFIFOs2 :: *;

// ----------------
// Bluespec Convey libs

import BC_Utils      :: *;
import BC_HW_IFC     :: *;
import BC_Common     :: *;

// ================================================================

export BC_Mem_Client, BC_Mem_Server,
       MemTreeNode (..), mkMemTreeNode;

// ================================================================
// Interfaces

typedef Client #(BC_MC_REQ, BC_MC_RSP) BC_Mem_Client;
typedef Server #(BC_MC_REQ, BC_MC_RSP) BC_Mem_Server;

// ================================================================
// MemTrees have requests flowing up (to root),
// responses flowing down (to leaves)

interface MemTreeNode #(numeric type m, numeric type n);
   method Action init (BC_AEId aeid);
   interface Vector #(n, BC_Mem_Client)  rootside;
   interface Vector #(m, BC_Mem_Server)  leafside;
endinterface

// If (! isRoot), then a request may flow up any upward link
// If (isRoot), then n must be 8 or 16 (i.e., # of Convey MC ports)
//     and a request may only flow up on channel 'addr[8:6]'

module mkMemTreeNode #(Bool isRoot) (MemTreeNode #(m, n))
   provisos (Log #(n, log_n),
	     Log #(m, log_m),
	     Max #(m, n, mn),
	     Log #(mn, log_mn),
	     Add #(c_, log_n, log_mn),
	     Add #(m, a_, mn),     // Compiler ought to know this
	     Add #(n, b_, mn));    // Compiler ought to know this
		    
   // Compile-time error check
   if (isRoot && (valueOf(n) != 8) && (valueOf(n) != 16))
      errorM ("ERROR: mkMemTreeNode: isRoot, but n != 8 and n != 16");

   // Some symbolic names for local constants
   Integer i_log_m = valueOf (log_m);

   Reg #(BC_AEId) rg_aeid <- mkRegU;

   // ----------------------------------------------------------------
   // Request net
   // Requests are sent upward on any available path if not root
   // or specific upward port if root

   // Request input bufs, output bufs, and ring buf
   Vector #(mn, FIFOF #(BC_MC_REQ))  vf_req_ins;
   Vector #(mn, FIFOF #(BC_MC_REQ))  vf_req_outs;
   Vector #(mn, Reg #(Maybe #(BC_MC_REQ))) vrg_req_buf <- replicateM (mkInvalidReg);

   for (Integer j = 0; j < valueOf (mn); j = j + 1) begin
      vf_req_ins  [j] <- (  (j < valueOf (m))
			  ? mkSpecialFIFOF (SchedPipeline, (! ugenq), ugdeq)
			  : mkDummyFIFOF);

      vf_req_outs [j] <- (  (j < valueOf (n))
			  ? mkSpecialFIFOF(SchedPipeline, ugenq, (! ugdeq))
			  : mkDummyFIFOF);
   end

   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_move_reqs;
      for (Integer j = 0; j < valueOf (mn); j = j + 1) begin
	 let jprev = ( (j == 0) ? (valueOf (mn) - 1) : (j - 1));
	 let mreq = vrg_req_buf [jprev];
	 if (jprev < valueOf (n)) begin
	    Bit #(log_mn) j1 = fromInteger (jprev);
	    if (     mreq matches tagged Valid .req
		&&&  is_at_rootside_destination (isRoot, req.vadr, valueOf (n), jprev)
		&&&  vf_req_outs [jprev].notFull)
	       begin
		  vf_req_outs [jprev].enq (req);
		  mreq = tagged Invalid;
	       end
	 end
	 if (mreq matches tagged Invalid  &&&  vf_req_ins [j].notEmpty) begin
	    let req = vf_req_ins [j].first; vf_req_ins [j].deq;
	    let req1 = BC_MC_REQ {cmd_sub: req.cmd_sub,
				  len    : req.len,
				  vadr   : req.vadr,
				  rtnctl : ((req.rtnctl << i_log_m) | fromInteger (j)),
				  data   : req.data};
	    mreq = tagged Valid req1;
	 end
	 vrg_req_buf [j] <= mreq;
      end
   endrule

   // ----------------------------------------------------------------
   // Response net
   // Responses are routed according to the Bit #(32) "return address stack"

   // Response input bufs, output bufs, and ring buf
   Vector #(mn, FIFOF #(BC_MC_RSP))  vf_rsp_ins;
   Vector #(mn, FIFOF #(BC_MC_RSP))  vf_rsp_outs;
   Vector #(mn, Reg #(Maybe #(BC_MC_RSP))) vrg_rsp_buf <- replicateM (mkInvalidReg);

   for (Integer j = 0; j < valueOf (mn); j = j + 1) begin
      vf_rsp_ins  [j] <- (  (j < valueOf (n))
			  ? mkSpecialFIFOF (SchedPipeline, (! ugenq), ugdeq)
			  : mkDummyFIFOF);

      vf_rsp_outs [j] <- (  (j < valueOf (m))
			  ? mkSpecialFIFOF (SchedPipeline, ugenq, (! ugdeq))
			  : mkDummyFIFOF);
   end

   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_move_rsps;
      for (Integer j = 0; j < valueOf (mn); j = j + 1) begin
	 let jprev = ( (j == 0) ? (valueOf (mn) - 1) : (j - 1));
	 let mrsp = vrg_rsp_buf [jprev];
	 if (jprev < valueOf (m)) begin
	    Bit #(log_m) j1 = fromInteger (jprev);
	    if (     mrsp matches tagged Valid .rsp
		&&&  is_at_leafside_destination (rsp.rtnctl, j1)  // (rtnctl [i_log_m - 1:0]  == j1)
		&&&  vf_rsp_outs [jprev].notFull)
	       begin
		  vf_rsp_outs [jprev].enq (BC_MC_RSP {cmd: rsp.cmd,
						      sub: rsp.sub,
						      rtnctl: (rsp.rtnctl >> i_log_m),
						      data: rsp.data});
		  mrsp = tagged Invalid;
	       end
	 end
	 if (mrsp matches tagged Invalid  &&&  vf_rsp_ins [j].notEmpty) begin
	    let rsp = vf_rsp_ins [j].first; vf_rsp_ins [j].deq;
	    mrsp = tagged Valid rsp;
	 end
	 vrg_rsp_buf [j] <= mrsp;
      end
   endrule

   // ----------------------------------------------------------------
   // INTERFACE
   
   function Client #(BC_MC_REQ, BC_MC_RSP)  fn_c (Integer j);
      return fifofs_to_client (vf_req_outs [j], vf_rsp_ins [j]);
   endfunction

   function Server #(BC_MC_REQ, BC_MC_RSP)  fn_s (Integer j);
      return fifofs_to_server (vf_req_ins [j], vf_rsp_outs [j]);
   endfunction

   method Action init (BC_AEId aeid);
      rg_aeid <= aeid;
   endmethod

   interface rootside = genWith (fn_c);
   interface leafside = genWith (fn_s);
endmodule

// ================================================================
// Internal utilities

// ----------------
// Checks if jprev is a root-side destination channel for addr
// If (! isRoot), then a request may flow up any upward link
// If (isRoot), then n must be 8 or 16 (i.e., # of Convey MC ports)
//     and a request may only flow up on channel 'addr[8:6]'

function Bool is_at_rootside_destination (Bool isRoot, BC_Addr addr, Integer int_n, Integer jprev);
   return (   (! isRoot)
	   || ((int_n ==  8) && (addr [8:6] == fromInteger (jprev)))
	   || ((int_n == 16) && (addr [8:6] == fromInteger (jprev / 2))));
endfunction

// ----------------
// Checks if jprev is a leaf-side destination channel for given tag
//     If int_m is 1, trivially true
//     If int_m > 1, the lower-order log(m) bits of tag should be == jprev

function Bool is_at_leafside_destination (Bit #(32) b32, Bit #(log_m) j1);
   Bool result = True;
   if (valueOf (log_m) != 0) begin
      Integer i_log_m = valueOf (log_m);
      result = (b32 [i_log_m - 1: 0] == j1);
   end
   return result;
endfunction

// ================================================================

endpackage
