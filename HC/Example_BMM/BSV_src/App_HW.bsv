// Copyright (c) 2013-2015 Bluespec, Inc.  All Rights Reserved.
//
// Author: Rishiyur Nikhil, Bluespec, Inc.

package App_HW;

// ================================================================
// Blocked matrix-multiplication, using Bluespec Convey library (BClib).

// The input and output matrices are organized into blocks, where each
// block is itself a (small) matrix of scalars.  The code reads and
// writes an entire block at a time, performing local matrix-multiply
// and matrix-add of blocks (without further memory refs), thereby
// reducing the total number of memory refs.  If the matrix sizes are
// NxN, and the block sizes are BxB, the total number of reads reduces
// from N^3 to (N^3)/B.

// See MatrixMult.bsv for greater detail.

// This code manages memory routing and ordering on its own.  Thus,
// when building the Convey bitfile, the PDK Makefile variables
// MC_XBAR and MC_READ_ORDER should NOT be set.

// ================================================================
// BSV Library imports

import Vector          :: *;
import FIFOF           :: *;
import GetPut          :: *;
import ClientServer    :: *;
import StmtFSM         :: *;

// ----------------
// BC library imports

import BC_Utils        :: *;
import BC_HW_IFC       :: *;
import BC_Transactors  :: *;

// ================================================================
// Project imports

import MatrixMult :: *;

// ================================================================
// Application-specific aeg count

Integer aeg_cnt = 1;    // We use only 1 AEG register!

// ================================================================
// Top-level of the app (for 1 FPGA)

(* synthesize *)
module mkApp_HW (BC_HW_IFC);

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
   
   MatrixMult_IFC matMult <- mkMatrixMult;

   // ----------------------------------------------------------------
   // Main behavior

   mkAutoFSM (
      seq
	 while (True) seq
	    $display ("%0d: mkApp_HW [%0d]: awaiting caep instruction", cur_cycle, aeid);
	    action
	       // The following just waits for a caep instruction; we don't have to actually
	       // examine it since we're implementing only one op (CAEP00) with no immediates etc.
	       let inst <- simple_dispatch_app_ifc.get_caep_inst.get;
	    endaction
	    action
	       // Read the arg in AEG 0
	       BC_Data arg = simple_dispatch_app_ifc.rd_aeg (0);
	       // Compute pArgv for this FPGA (for each FPGA we reserve 256 words of 8 bytes)
	       BC_Addr aeid1 = extend (aeid);
	       BC_Addr argv = truncate (arg) + (aeid1 << 11);

	       // Start matMult with argv
	       $display ("%0d: mkApp_HW [%0d]: Starting matMult with arg 0x%0h", cur_cycle, aeid, argv);
	       matMult.start (aeid, argv);
	    endaction
	    matMult.waitTillDone;
	    delay (100);    // to allow final writes to reach memory.
	    simple_dispatch_app_ifc.put_caep_result.put (0); // signal caep inst completion
	 endseq
      endseq
      );

   // ================================================================
   // INTERFACE

   interface mc_ifcs        = matMult.mc_ifcs;
   interface dispatch_ifc   = top_dispatch_ifc;
   interface management_ifc = top_management_ifc;
endmodule: mkApp_HW

// ================================================================

endpackage
