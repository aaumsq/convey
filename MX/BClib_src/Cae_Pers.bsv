// Copyright (c) 2012-2015 Bluespec, Inc.,  All Rights Reserved.
// Distributed under license.

// Author: Rishiyur S. Nikhil

package Cae_Pers;

// ================================================================
// This defines BSV module 'cae_pers' which is the top-level module for
// deploying on Convey HW.  When compiled, it produces a Verilog file
// 'cae_pers.v' containing a Verilog module 'cae_pers' that is required
// by, and instantiated by, Convey's PDK.

// This 'cae_pers' module instantiates the user's BSV 'mkApp_HW'.

// It is not used during Bluesim simulation of the user's mkApp_HW,
// which does not use Convey's PDK. (see BC_Sim_Main.bsv for the
// top-level used for Bluesim).

// It is only used when building for Convey hardware or Convey PDK
// simulation (RTL simulation) where, of course, the generated Verilog
// connects to Convey's PDK.

// December 2014: This is a new version that directly uses the
// "positive" reset provided by Convey's PDK, and eliminating the
// earlier "reset inverter" mechanism.

// This version is intended for Type 1 memory interfaces.

// ================================================================

// BSV library imports
import Vector    :: *;
import FIFOF     :: *;
import FIFOLevel :: *;
import Clocks    :: *;


// BClib imports
import BC_HW_IFC             :: *;
import BC_Dispatch_Adapter   :: *;
import BC_MC_Adapter         :: *;
import BC_Management_Adapter :: *;


// The user-supplied app
import App_HW                :: *;

// ================================================================
// Verilog MC port naming in Type 1 memory interfaces.
//
// The following is an excerpt from the Verilog port list of the PDK
// example vector-add application, showing how the Verilog ports are
// named.
//
//     module cae_pers #(parameter    NUM_MC_PORTS = 16) (
//        ...
//        //
//        // MC Interface(s)
//        //
//        output [NUM_MC_PORTS*1-1 :0]         mc_rq_vld,
//        output [NUM_MC_PORTS*32-1:0]         mc_rq_rtnctl,
//        output [NUM_MC_PORTS*64-1:0]         mc_rq_data,
//        output [NUM_MC_PORTS*48-1:0]         mc_rq_vadr,
//        output [NUM_MC_PORTS*2-1 :0]         mc_rq_len,
//        output [NUM_MC_PORTS*4-1 :0]         mc_rq_sub,
//        output [NUM_MC_PORTS*3-1 :0]         mc_rq_cmd,
//        input  [NUM_MC_PORTS*1-1 :0]         mc_rq_stall,
//        
//        input  [NUM_MC_PORTS*1-1 :0]         mc_rs_vld,
//        input  [NUM_MC_PORTS*3-1 :0]         mc_rs_cmd,
//        input  [NUM_MC_PORTS*3-1 :0]         mc_rs_sub,
//        input  [NUM_MC_PORTS*64-1:0]         mc_rs_data,
//        input  [NUM_MC_PORTS*32-1:0]         mc_rs_rtnctl,
//        output [NUM_MC_PORTS*1-1 :0]         mc_rs_stall,
//        ...
//     endmodule
//
// Thus the Verilog port names are simply wider by 16x, with the same names
// as on each of the 16 individual interfaces.
// Specifically, each of the above ports represents a concatenation of
// the corresponding ports for each corresponding interface.
// For example:
//     mc_rq_data [  0: 63] are the signals for port 0,
//     mc_rq_data [ 64:127] are the signals for port 1,
//     mc_rq_data [128:191] are the signals for port 2,
//     ...
//     mc_rq_data [j*64 +: 64] are the signals for port j

// ================================================================

interface Cae_Pers_IFC;
   // ----------------------------------------------------------------
   // Misc interfaces

   (* prefix="", always_ready, always_enabled *)
   method Action misc_inputs (Bit#(1) clk_csr, Bit #(1) clk2x, Bit #(1) i_csr_reset_n, Bit #(1) ppll_reset);

   (* prefix="", always_ready *)
   method Bit#(1) ppll_locked;

   interface Clock clk_per;

   (* prefix="", always_ready, always_enabled *)
   method Action m_set_aeid (Bit#(2) i_aeid);

   // ----------------------------------------------------------------
   // Dispatch Interface
   (* prefix="" *)     interface BC_Dispatch_Signal_IFC dispatch_signal_ifc;

   // ----------------------------------------------------------------
   // MC Interfaces

   // Request Port
   (* always_ready *)  method Vector #(16, Bit #(1))   mc_rq_vld;
   (* always_ready *)  method Vector #(16, Bit #(3))   mc_rq_cmd;
   (* always_ready *)  method Vector #(16, Bit #(4))   mc_rq_sub;
   (* always_ready *)  method Vector #(16, Bit #(2))   mc_rq_len;
   (* always_ready *)  method Vector #(16, Bit #(48))  mc_rq_vadr;
   (* always_ready *)  method Vector #(16, Bit #(32))  mc_rq_rtnctl;
   (* always_ready *)  method Vector #(16, Bit #(64))  mc_rq_data;
   (* always_ready,
      always_enabled,
      prefix = "" *)   method Action m_rq_stall (Vector #(16, Bit #(1)) mc_rq_stall);

   // Response Port
   (* always_ready,
      always_enabled,
      prefix = "" *)   method Action m_rsp (Vector #(16, Bit #(1))  mc_rs_vld,
					    Vector #(16, Bit #(3))  mc_rs_cmd,
					    Vector #(16, Bit #(3))  mc_rs_sub,
					    Vector #(16, Bit #(32)) mc_rs_rtnctl,
					    Vector #(16, Bit #(64)) mc_rs_data);

   (* always_ready *)  method Vector #(16, Bit #(1)) mc_rs_stall;

   // Write Flush
   (* always_ready *)  method Vector #(16, Bit #(1)) mc_rq_flush;
   (* always_ready,
      always_enabled,
      prefix = "" *)   method Action m_rs_flush_cmplt (Vector #(16, Bit #(1)) mc_rs_flush_cmplt);

   // ----------------------------------------------------------------
   // Management Interface
   (* prefix="" *)      interface BC_Management_Signal_IFC management_signal_ifc;
endinterface

// ================================================================
// The top-level module, instantiated by Convey's PDK
// (with Type 1 memory interfaces)

// reset_prefix = "",

(* synthesize, clock_prefix = "",
   default_clock_osc = "clk",
   default_gate_inhigh,
   default_reset = "i_reset" *)
module cae_pers (Cae_Pers_IFC);

   // ----------------------------------------------------------------
   // Instantiate the BSV app
   BC_HW_IFC  app <- mkApp_HW;

   // ----------------------------------------------------------------
   Reg #(Bool) rg_aeid_has_been_set <- mkReg (False);

   Clock cur_clk <- exposeCurrentClock;

   // ----------------------------------------------------------------
   // Adapt the dispatch interface from BSV to signals
   BC_Dispatch_Signal_IFC  dispatch <- mkBC_Dispatch_Adapter (app.dispatch_ifc);

   // ----------------------------------------------------------------
   // MC adapters

   Vector #(16, BC_MC_Port_Signal_IFC) mc_ifcs;
   for (Integer j = 0; j < 16; j = j + 1)
      mc_ifcs [j] <- mkBC_MC_port_adapter (app.mc_ifcs [j]);

   // ----------------------------------------------------------------
   // Management interface adapter

   BC_Management_Signal_IFC  mgmt <- mkBC_Management_adapter (app.management_ifc);

   // ================================================================
   // Interface

   // ----------------------------------------------------------------
   // Misc interfaces

   method Action misc_inputs (Bit#(1) clk_csr, Bit #(1) clk2x, Bit #(1) i_csr_reset_n, Bit #(1) ppll_reset) = noAction;

   method Bit#(1) ppll_locked = 1'b1;

   interface Clock clk_per = cur_clk;

   method Action m_set_aeid (Bit#(2) i_aeid);
      if (! rg_aeid_has_been_set)    // Convert continuous signal to a one-time transaction
	 app.management_ifc.set_aeid (i_aeid);
      rg_aeid_has_been_set <= True;
   endmethod

   // ----------------
   // Dispatch Interface
   interface BC_Dispatch_Signal_IFC dispatch_signal_ifc = dispatch;

   // ----------------
   // MC Interfaces

   method mc_rq_vld;
      Vector #(16, Bit #(1)) v;
      for (Integer j = 0; j < 16; j = j + 1) v [j] = mc_ifcs [j].mc_req_vld;
      return v;
   endmethod

   method mc_rq_cmd;
      Vector #(16, Bit #(3)) v;
      for (Integer j = 0; j < 16; j = j + 1) v [j] = mc_ifcs [j].mc_req_cmd;
      return v;
   endmethod

   method mc_rq_sub;
      Vector #(16, Bit #(4)) v;
      for (Integer j = 0; j < 16; j = j + 1) v [j] = mc_ifcs [j].mc_req_sub;
      return v;
   endmethod

   method mc_rq_len;
      Vector #(16, Bit #(2)) v;
      for (Integer j = 0; j < 16; j = j + 1) v [j] = mc_ifcs [j].mc_req_len;
      return v;
   endmethod

   method mc_rq_vadr;
      Vector #(16, Bit #(48)) v;
      for (Integer j = 0; j < 16; j = j + 1) v [j] = mc_ifcs [j].mc_req_vadr;
      return v;
   endmethod

   method mc_rq_rtnctl;
      Vector #(16, Bit #(32)) v;
      for (Integer j = 0; j < 16; j = j + 1) v [j] = mc_ifcs [j].mc_req_rtnctl;
      return v;
   endmethod

   method mc_rq_data;
      Vector #(16, Bit #(64)) v;
      for (Integer j = 0; j < 16; j = j + 1) v [j] = mc_ifcs [j].mc_req_data;
      return v;
   endmethod

   method Action m_rq_stall (Vector #(16, Bit #(1)) mc_rq_stall);
      for (Integer j = 0; j < 16; j = j + 1)
	 mc_ifcs [j].m_req_stall (mc_rq_stall [j]);
   endmethod

   // Response Port
   method Action m_rsp (Vector #(16, Bit #(1))  mc_rs_vld,
			Vector #(16, Bit #(3))  mc_rs_cmd,
			Vector #(16, Bit #(3))  mc_rs_sub,
			Vector #(16, Bit #(32)) mc_rs_rtnctl,
			Vector #(16, Bit #(64)) mc_rs_data);
      for (Integer j = 0; j < 16; j = j + 1)
	 mc_ifcs [j].m_rsp (mc_rs_vld [j], mc_rs_cmd [j], mc_rs_sub [j], mc_rs_rtnctl [j], mc_rs_data [j]);
   endmethod

   method mc_rs_stall;
      Vector #(16, Bit #(1)) v;
      for (Integer j = 0; j < 16; j = j + 1) v [j] = mc_ifcs [j].mc_rsp_stall;
      return v;
   endmethod

   // Write Flush
   method mc_rq_flush;
      Vector #(16, Bit #(1)) v;
      for (Integer j = 0; j < 16; j = j + 1) v [j] = mc_ifcs [j].mc_req_flush;
      return v;
   endmethod

   method Action m_rs_flush_cmplt (Vector #(16, Bit #(1)) mc_rs_flush_cmplt);
      for (Integer j = 0; j < 16; j = j + 1)
	 mc_ifcs [j].m_rsp_flush_cmplt (mc_rs_flush_cmplt [j]);
   endmethod

   // ----------------
   // Management Interface

   interface BC_Management_Signal_IFC management_signal_ifc = mgmt;
endmodule

endpackage
