// Copyright (c) 2015, The University of Texas at Austin.  All Rights Reserved.
//
// Author: Dan Zhang

package App_HW;

// ================================================================
// This is Version 0 of the Vector-Add example for the Bluespec-Convey
// library (BClib).  Here, we assume that all three vectors are
// identically aligned w.r.t. the MCs.  Hence, memory requests and
// responses go to statically specified MCs.

// This version assumes ordered memory read-responses, i.e., the Convey PDK
// Verilog should be given the flag MC_READ_ORDER=1

// On Convey HC-1 machines, it runs as fast as Convey's PDK vector-add example,
// when tested with vector sizes from 100, 1000, ..., 1 Billion

// ================================================================
// BSV Library imports

import Vector           :: *;
import FIFOF            :: *;
import GetPut           :: *;
import ClientServer     :: *;
import Connectable      :: *;
import StmtFSM          :: *;
import FShow            :: *;
import BRAMFIFO         :: *;

// ----------------
// BC library imports

import BC_Utils           :: *;
import BC_HW_IFC          :: *;
import BC_Transactors     :: *;


import SSSP::*;
import Clocks::*;

// ================================================================
// Application-specific aeg count

Integer aeg_cnt = 1;    // We use only 1 AEG register!

// ================================================================
// Top-level of the app (replicated for each FPGA)

(* synthesize *)
module mkApp_HW (BC_HW_IFC);

    Clock clk <- exposeCurrentClock;
    Reset rst <- exposeCurrentReset;
    
    MakeResetIfc ssspRst <- mkReset(1, False, clk);

   // ----------------------------------------------------------------
   // Instantiate a null management transactor

   match { .top_management_ifc, .management_app_ifc } <- mkBC_Management_Null_Transactor;

   // Get the fpga number (aeid) from the transactor
   BC_AEId  aeid = management_app_ifc.aeid;

   // ----------------------------------------------------------------
   // Instantiate Simple Decode/Dispatch transactor with aeg_cnt registers

   match { .top_dispatch_ifc, .simple_dispatch_app_ifc } <- mkSimple_Dispatch (fromInteger (aeg_cnt - 1));

   rule rl_propagate_aeid;
      simple_dispatch_app_ifc.set_aeid (aeid);
   endrule

   // ----------------------------------------------------------------
   // Instantiate the App and use its mem ports directly
   // (assumes ordered memory responses)
   
    BC_HW2_IFC hw2 <- mkSSSP(reset_by ssspRst.new_rst); //mkApp_HW2;

   // ----------------------------------------------------------------
   // Main behavior

   mkAutoFSM (
      seq
	 // Loop forever, executing caep instructions
	 while (True) seq
	    $display ("%0d: mkApp_HW [%0d]: awaiting caep instruction", cur_cycle, aeid);
        action
            ssspRst.assertReset();
        endaction
        action
            noAction;
        endaction
	    action
	       // The following just waits for a caep instruction; we don't have to actually
	       // examine it since we're implementing only one op (CAEP00) with no immediates etc.
	       let inst <- simple_dispatch_app_ifc.get_caep_inst.get;
	    endaction
	    action
	       // Read the arg in AEG 0 and start hw2 with this arg
	       BC_Data arg = simple_dispatch_app_ifc.rd_aeg (0);
	       $display ("%0d: mkApp_HW [%0d]: Starting HW2 with arg 0x%0h", cur_cycle, aeid, arg);
	       hw2.start (aeid, arg);
	    endaction

	    hw2.waitTillDone;

        // signal caep inst completion
	    simple_dispatch_app_ifc.put_caep_result.put (0);
	 endseq
      endseq
      );

   // ================================================================
   // INTERFACE

   interface mc_ifcs        = hw2.mc_ifcs;
   interface dispatch_ifc   = top_dispatch_ifc;
   interface management_ifc = top_management_ifc;
endmodule: mkApp_HW

endpackage
