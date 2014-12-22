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

// This version is intended for Type 0 memory interfaces.

// ================================================================

// BSV library imports
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
   (* prefix="" *)      interface BC_Dispatch_Signal_IFC dispatch_signal_ifc;

   // ----------------------------------------------------------------
   // MC Interfaces

   (* prefix="mc0" *)   interface BC_MC_Port_Signal_IFC_e  mc0_e;
   (* prefix="mc0" *)   interface BC_MC_Port_Signal_IFC_o  mc0_o;

   (* prefix="mc1" *)   interface BC_MC_Port_Signal_IFC_e  mc1_e;
   (* prefix="mc1" *)   interface BC_MC_Port_Signal_IFC_o  mc1_o;

   (* prefix="mc2" *)   interface BC_MC_Port_Signal_IFC_e  mc2_e;
   (* prefix="mc2" *)   interface BC_MC_Port_Signal_IFC_o  mc2_o;

   (* prefix="mc3" *)   interface BC_MC_Port_Signal_IFC_e  mc3_e;
   (* prefix="mc3" *)   interface BC_MC_Port_Signal_IFC_o  mc3_o;

   (* prefix="mc4" *)   interface BC_MC_Port_Signal_IFC_e  mc4_e;
   (* prefix="mc4" *)   interface BC_MC_Port_Signal_IFC_o  mc4_o;

   (* prefix="mc5" *)   interface BC_MC_Port_Signal_IFC_e  mc5_e;
   (* prefix="mc5" *)   interface BC_MC_Port_Signal_IFC_o  mc5_o;

   (* prefix="mc6" *)   interface BC_MC_Port_Signal_IFC_e  mc6_e;
   (* prefix="mc6" *)   interface BC_MC_Port_Signal_IFC_o  mc6_o;

   (* prefix="mc7" *)   interface BC_MC_Port_Signal_IFC_e  mc7_e;
   (* prefix="mc7" *)   interface BC_MC_Port_Signal_IFC_o  mc7_o;

   // ----------------------------------------------------------------
   // Management Interface
   (* prefix="" *)      interface BC_Management_Signal_IFC management_signal_ifc;
endinterface

// ================================================================
// The top-level module, instantiated by Convey's PDK
// (with Type 0 memory interfaces)

(* synthesize, clock_prefix = "", reset_prefix = "",
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

   BC_MC_Port_Signal_IFC   mc0_e_signal_ifc <- mkBC_MC_port_adapter (tpl_1 (app.mc_ifcs [0]));
   BC_MC_Port_Signal_IFC   mc0_o_signal_ifc <- mkBC_MC_port_adapter (tpl_2 (app.mc_ifcs [0]));

   BC_MC_Port_Signal_IFC   mc1_e_signal_ifc <- mkBC_MC_port_adapter (tpl_1 (app.mc_ifcs [1]));
   BC_MC_Port_Signal_IFC   mc1_o_signal_ifc <- mkBC_MC_port_adapter (tpl_2 (app.mc_ifcs [1]));

   BC_MC_Port_Signal_IFC   mc2_e_signal_ifc <- mkBC_MC_port_adapter (tpl_1 (app.mc_ifcs [2]));
   BC_MC_Port_Signal_IFC   mc2_o_signal_ifc <- mkBC_MC_port_adapter (tpl_2 (app.mc_ifcs [2]));

   BC_MC_Port_Signal_IFC   mc3_e_signal_ifc <- mkBC_MC_port_adapter (tpl_1 (app.mc_ifcs [3]));
   BC_MC_Port_Signal_IFC   mc3_o_signal_ifc <- mkBC_MC_port_adapter (tpl_2 (app.mc_ifcs [3]));

   BC_MC_Port_Signal_IFC   mc4_e_signal_ifc <- mkBC_MC_port_adapter (tpl_1 (app.mc_ifcs [4]));
   BC_MC_Port_Signal_IFC   mc4_o_signal_ifc <- mkBC_MC_port_adapter (tpl_2 (app.mc_ifcs [4]));

   BC_MC_Port_Signal_IFC   mc5_e_signal_ifc <- mkBC_MC_port_adapter (tpl_1 (app.mc_ifcs [5]));
   BC_MC_Port_Signal_IFC   mc5_o_signal_ifc <- mkBC_MC_port_adapter (tpl_2 (app.mc_ifcs [5]));

   BC_MC_Port_Signal_IFC   mc6_e_signal_ifc <- mkBC_MC_port_adapter (tpl_1 (app.mc_ifcs [6]));
   BC_MC_Port_Signal_IFC   mc6_o_signal_ifc <- mkBC_MC_port_adapter (tpl_2 (app.mc_ifcs [6]));

   BC_MC_Port_Signal_IFC   mc7_e_signal_ifc <- mkBC_MC_port_adapter (tpl_1 (app.mc_ifcs [7]));
   BC_MC_Port_Signal_IFC   mc7_o_signal_ifc <- mkBC_MC_port_adapter (tpl_2 (app.mc_ifcs [7]));

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

   interface BC_MC_Port_Signal_IFC_e mc0_e = to_mc_port_e (mc0_e_signal_ifc);
   interface BC_MC_Port_Signal_IFC_e mc0_o = to_mc_port_o (mc0_o_signal_ifc);

   interface BC_MC_Port_Signal_IFC_e mc1_e = to_mc_port_e (mc1_e_signal_ifc);
   interface BC_MC_Port_Signal_IFC_e mc1_o = to_mc_port_o (mc1_o_signal_ifc);

   interface BC_MC_Port_Signal_IFC_e mc2_e = to_mc_port_e (mc2_e_signal_ifc);
   interface BC_MC_Port_Signal_IFC_e mc2_o = to_mc_port_o (mc2_o_signal_ifc);

   interface BC_MC_Port_Signal_IFC_e mc3_e = to_mc_port_e (mc3_e_signal_ifc);
   interface BC_MC_Port_Signal_IFC_e mc3_o = to_mc_port_o (mc3_o_signal_ifc);

   interface BC_MC_Port_Signal_IFC_e mc4_e = to_mc_port_e (mc4_e_signal_ifc);
   interface BC_MC_Port_Signal_IFC_e mc4_o = to_mc_port_o (mc4_o_signal_ifc);

   interface BC_MC_Port_Signal_IFC_e mc5_e = to_mc_port_e (mc5_e_signal_ifc);
   interface BC_MC_Port_Signal_IFC_e mc5_o = to_mc_port_o (mc5_o_signal_ifc);

   interface BC_MC_Port_Signal_IFC_e mc6_e = to_mc_port_e (mc6_e_signal_ifc);
   interface BC_MC_Port_Signal_IFC_e mc6_o = to_mc_port_o (mc6_o_signal_ifc);

   interface BC_MC_Port_Signal_IFC_e mc7_e = to_mc_port_e (mc7_e_signal_ifc);
   interface BC_MC_Port_Signal_IFC_e mc7_o = to_mc_port_o (mc7_o_signal_ifc);

   // ----------------
   // Management Interface

   interface BC_Management_Signal_IFC management_signal_ifc = mgmt;
endmodule

endpackage
