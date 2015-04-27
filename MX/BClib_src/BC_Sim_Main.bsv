// Copyright (c) 2011-2015 Bluespec, Inc.  All Rights Reserved.
// Distributed under license.

// Author: Rishiyur S. Nikhil

package BC_Sim_Main;

// This package defines a top-level module mkBC_Sim_Main
// which embeds the application's C code and BSV code
// to create a standalone Bluesim simulation
// for testing prior to deployment on actual Convey hardware.

// In this version, the memory interfaces are Type 1 memory (e.g., MX platform)

// ================================================================
// BSV lib imports

import RegFile          :: *;
import Vector           :: *;
import FIFOF            :: *;
import GetPut           :: *;
import ClientServer     :: *;
import StmtFSM          :: *;
import Connectable      :: *;
import FShow            :: *;

// ----------------
// BClib imports

import BC_Utils      :: *;
import BC_HW_IFC     :: *;
import BC_BDPI_decls :: *;
import BC_MC_Model   :: *;

// ----------------
// Local (app-specific) imports

import App_HW :: *;

// ================================================================
// Functions to construct Convey 32b instructions

function BC_Inst bc_instwr_aeg (BC_Aeg_Idx idx);
   Bit #(18) idx18b = extend (idx);
   return extend ({ 5'b11100, 6'h18, idx18b});
endfunction

function BC_Inst bc_instrd_aeg (BC_Aeg_Idx idx);
   Bit #(12) idx12b = extend (idx);
   return extend ({5'b11101, 6'h1C, idx12b, 6'h0});
endfunction

BC_Inst bc_instcaep0   = extend ({ 5'b11110, 1'b1, 23'h0});

// ================================================================
// This is the top-level harness for standalone Bluesim simulation of
// the application C code and BSV code

(* synthesize *)
module mkBC_Sim_Main (Empty);

   // Instantiate mkApp_HW for each of the 4 FPGAs
   Vector #(BC_Num_FPGAs, BC_HW_IFC) hw_modules <- replicateM (mkApp_HW);

   // Instantiate 16 mem controllers per FPGA, connect to corresponding dut ports
   Vector #(BC_Num_FPGAs, Vector #(16, BC_MC_Model_IFC)) vv_mc_models = ?;

   for (Integer fpga = 0; fpga < bc_num_FPGAs; fpga = fpga + 1) begin
      for (Integer mc = 0; mc < 16; mc = mc + 1) begin
	 let mc_model <- mkBC_MC_Model (fromInteger (fpga), fromInteger (mc));
	 mkConnection (hw_modules [fpga].mc_ifcs [mc], mc_model.server);
	 vv_mc_models [fpga] [mc] = mc_model;
      end

      // Drive aeid, csr_31_32_intlv_dis
      rule rl_always;
	 hw_modules [fpga].management_ifc.set_aeid (fromInteger (fpga));
	 hw_modules [fpga].management_ifc.m_csr_31_31_intlv_dis (True);
      endrule

      // Dummy drive ring messages
      rule rl_always2;
	 hw_modules [fpga].management_ifc.put_ring_msg_in.put (?);
      endrule

      // Dummy drain ring messages
      rule rl_always3;
	 let x <- hw_modules [fpga].management_ifc.get_ring_msg_out.get;
      endrule
   end

   Reg #(BC_Data)    rg_arg         <- mkRegU;
   Reg #(Bit #(64))  rg_init_cycle  <- mkReg (0);
   Reg #(Bit #(64))  rg_final_cycle <- mkReg (0);

   Reg #(Vector #(4, BC_ExceptionVec)) rg_v_ev <- mkReg (replicate (0));

   mkAutoFSM (
      seq
	 // Start the SW computation (in C)
	 bc_create_C_thread;

	 // Initialize the memory models to stop on wrong-bank and unaligned accesses
	 action
	    for (Integer fpga = 0; fpga < bc_num_FPGAs; fpga = fpga + 1)
	      for (Integer mc = 0; mc < 16; mc = mc + 1)
		 vv_mc_models [fpga] [mc].set_params (128,      // latency
						      1,      // debug_mem_trace_verbosity
						      False,  // debug_stop_on_wrong_bank
						      True);  // debug_stop_on_unaligned
	 endaction

	 // Receive 64-bit arg from C
	 action
	    Bit #(64) arg <- bc_recv_C_to_BSV;
	    $display ("%0d: BC_Sim_Main: received arg from C: 0x%0h", cur_cycle, arg);
	    rg_arg <= arg;
	 endaction

	 // Load arg into AEG [0]
	 action
	    for (Integer fpga = 0; fpga < bc_num_FPGAs; fpga = fpga + 1)
	       hw_modules [fpga].dispatch_ifc.put_inst.put (BC_Dispatch_inst {inst: bc_instwr_aeg (0),  data: rg_arg});
	 endaction

	 // Start the CAEP computation on each FPGA
	 action
	    for (Integer fpga = 0; fpga < bc_num_FPGAs; fpga = fpga + 1)
	       hw_modules [fpga].dispatch_ifc.put_inst.put (BC_Dispatch_inst {inst: bc_instcaep0, data: 0});
	    let init_cycle <- cur_cycle;
	    rg_init_cycle <= extend (init_cycle);
	 endaction

	 // Wait for final exception results from each FPGA and send OR'd vector back
	 action
	    let final_cycle <- cur_cycle;
	    Vector #(4, BC_ExceptionVec) v_ev;
	    for (Integer fpga = 0; fpga < bc_num_FPGAs; fpga = fpga + 1) begin
	       BC_ExceptionVec evJ <- hw_modules [fpga].dispatch_ifc.get_exception.get;
	       $display ("%0d: BC_Sim_Main: FPGA %0d: caep done; final exceptionvec 0x%0h", final_cycle, fpga, evJ);
	       v_ev [fpga] = evJ;
	    end
	    rg_v_ev <= v_ev;
	    rg_final_cycle <= extend (final_cycle);
	 endaction

	 // Wait for some cycles for writes to reach memory
	 delay (200);

	 action
	    // $display ("%0d: BC_Sim_Main: sending to C exceptionvecs ", rg_final_cycle, fshow (rg_v_ev));
	    bc_send_BSV_to_C (pack (rg_v_ev));
	 endaction

	 // Wait for the SW computation (in C) to exit
	 action
	    bc_join_C_thread;
	    $display ("%0d: BC_Sim_Main: joined C thread; all done; exiting", cur_cycle);
	 endaction

	 // Display mem stats
	 action
	    UInt #(64) tot_rds = 0;
	    UInt #(64) tot_wrs = 0;
	    UInt #(64) tot_ats = 0;
	    let cycle <- cur_cycle;
  		for (Integer fpga = 0; fpga < bc_num_FPGAs; fpga = fpga + 1) begin
	       for (Integer j = 0; j < 16; j = j + 1) begin
          match { .rds, .wrs, .ats } = vv_mc_models [fpga][j].rd_wr_counts;
		  $display ("BC_Sim_Main: FPGA [%1d] mem [%3d] accesses: %10d reads, %10d writes, %10d atomics, 0.%4d", fpga, j, rds, wrs, ats, 10000*(rds+wrs+ats)/extend(unpack(cycle)));
		  tot_rds = tot_rds + rds;
		  tot_wrs = tot_wrs + wrs;
		  tot_ats = tot_ats + ats;
	      end
	    end
	    $display ("BC_Sim_Main:          Total mem accesses: %10d reads, %10d writes, %10d atomics", tot_rds, tot_wrs, tot_ats);
        $display("BC_Sim_Main:          Memory utilization: 0.%4d", 10000*(tot_rds + tot_wrs + tot_ats)/(4*16*extend(unpack(cycle))));
	 endaction
      endseq
      );
endmodule

// ================================================================

endpackage
