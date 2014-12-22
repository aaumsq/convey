// Copyright (c) 2012-2015 Bluespec, Inc.  All Rights Reserved.
//
// Author: Rishiyur S. Nikhil

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

// ================================================================
// Application-specific aeg count

Integer aeg_cnt = 1;    // We use only 1 AEG register!

// ================================================================
// Top-level of the app (replicated for each FPGA)

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
   // (assumes ordered memory responses)
   
    BC_HW2_IFC hw2 <- mkSSSP; //mkApp_HW2;

   // ----------------------------------------------------------------
   // Main behavior

   mkAutoFSM (
      seq
	 // Loop forever, executing caep instructions
	 while (True) seq
	    $display ("%0d: mkApp_HW [%0d]: awaiting caep instruction", cur_cycle, aeid);
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

            // Must wait for writes to reach memory before signaling completion
	    delay (200);

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

// ================================================================
// HW module with simplified API

// ----------------


// ----------------
// Position of parameters in param block

Integer param_VSIZE       = 0;
Integer param_VIN1_P      = 1;
Integer param_VIN2_P      = 2;
Integer param_VOUT_P      = 3;
Integer param_PARTIAL_SUM = 4;

// ----------------

(* synthesize *)
module mkApp_HW2 (BC_HW2_IFC);
   Reg #(BC_AEId)         rg_fpga_id          <- mkRegU;
   Reg #(BC_Addr)         rg_param_block_addr <- mkRegU;
   Reg #(BC_Data)         rg_partial_sum      <- mkRegU;

   // ----------------------------------------------------------------
   // MC FIFOs

   Vector #(16, FIFOF #(BC_MC_REQ))       f_reqs       <- replicateM (mkFIFOF);
   Vector #(16, FIFOF #(BC_MC_RSP))       f_rsps       <- replicateM (mkFIFOF);
   Vector #(16, FIFOF #(BC_MC_flush_req)) f_flush_reqs <- replicateM (mkFIFOF);
   Vector #(16, FIFOF #(BC_MC_flush_rsp)) f_flush_rsps <- replicateM (mkFIFOF);

   // ----------------------------------------------------------------
   // Instantiate 16 vadders, and connect them to the MC fifos

   Vector #(16, ChanVadd_IFC) v_chanvadds <- replicateM (mkChanVadd);

   for (Integer mc = 0; mc < 16; mc = mc + 1)
      mkConnection (v_chanvadds [mc].mc_client,
		    fn_FIFOFs_to_MC_Server (f_reqs [mc],
					    f_rsps [mc],
					    f_flush_reqs [mc],
					    f_flush_rsps [mc]));

   // ----------------------------------------------------------------
   // The following are used for reading/writing from/to the parameter block
   // Note, they share the same req/rsp fifos with the mkChanVadd modules,
   // but there is no confusion because they are used at disjoint phases
   // of the FSM below.

   function Action send_rd_req (BC_Addr base, Integer param_id);
      action
	 let addr = base + fromInteger (param_id * 64);
	 let req = BC_MC_REQ {cmd_sub: REQ_RD, rtnctl: 0, len: BC_8B,
			      vadr: addr, data: ?};
	 f_reqs [param_id * 2].enq (req);
      endaction
   endfunction

   function Action send_wr_req (BC_Addr base, Integer param_id, BC_Data x);
      action
	 let addr = base + fromInteger (param_id * 64);
	 let req = BC_MC_REQ {cmd_sub: REQ_WR, rtnctl: 0, len: BC_8B,
			      vadr: addr, data: x};
	 f_reqs [param_id * 2].enq (req);
      endaction
   endfunction

   function ActionValue #(BC_Addr) recv_rd_rsp (Integer param_id);
      actionvalue
	 let rsp <- toGet (f_rsps [param_id * 2]).get;

	 // Assertion: the only responses in f_rsps[] should be RSP_RD_DATA responses
	 // for the parameter read requests
	 if (rsp.cmd != RSP_RD_DATA) begin
	    $display ("INTERNAL ERROR: mkApp_HW2: memory response for parameter-read is of wrong RS_TYPE",
		      fshow (rsp.cmd));
	    $display ("    Response is: ", fshow (rsp));
	    $finish (1);
	 end

	 return truncate (rsp.data);
      endactionvalue
   endfunction

   // ----------------------------------------------------------------
   // MAIN BEHAVIOR 

   Reg #(UInt #(5)) rg_mc <- mkRegU;

   let fsm <- mkFSM (
      seq
	 // Send read requests for the parameters for this FPGA (in parallel)
	 action
	    send_rd_req (rg_param_block_addr, param_VSIZE);
	    send_rd_req (rg_param_block_addr, param_VIN1_P);
	    send_rd_req (rg_param_block_addr, param_VIN2_P);
	    send_rd_req (rg_param_block_addr, param_VOUT_P);
	 endaction

	 // Receive the parameters for this FPGA (read responses, in parallel)
	 action
	    let vsize   <- recv_rd_rsp (param_VSIZE);
	    let vin1_p  <- recv_rd_rsp (param_VIN1_P);
	    let vin2_p  <- recv_rd_rsp (param_VIN2_P);
	    let vout_p  <- recv_rd_rsp (param_VOUT_P);
	    $display ("%0d: mkApp_HW2 [%0d]: params are %0d 0x%0h 0x%0h 0x%0h; starting per-MC vadds",
		      cur_cycle, rg_fpga_id, vsize, vin1_p, vin2_p, vout_p);

	    // Start the 16 vector-adder engines
	    for (Integer mc = 0; mc < 8; mc = mc + 1) action
	       v_chanvadds [2 * mc].start     (rg_fpga_id, fromInteger (mc),
					       True,
					       bc_base_addr_for_chan (mc, vin1_p),
					       vin1_p + (vsize << 3),
					       bc_base_addr_for_chan (mc, vin2_p),
					       vin2_p + (vsize << 3),
					       bc_base_addr_for_chan (mc, vout_p),
					       vout_p + (vsize << 3));
	       v_chanvadds [2 * mc + 1].start (rg_fpga_id, fromInteger (mc),
					       False,
					       8 + bc_base_addr_for_chan (mc, vin1_p),
					       vin1_p + (vsize << 3),
					       8 + bc_base_addr_for_chan (mc, vin2_p),
					       vin2_p + (vsize << 3),
					       8 + bc_base_addr_for_chan (mc, vout_p),
					       vout_p + (vsize << 3));
	    endaction
	    rg_partial_sum <= 0;
	 endaction

	 // Collect per-channel partial sums, accumulate into full (per-FPGA) sum
	 for (rg_mc <= 0; rg_mc < 16; rg_mc <= rg_mc + 1) action
	    let chan_sum <- v_chanvadds [rg_mc].result;
	    rg_partial_sum <= rg_partial_sum + chan_sum;
	 endaction

	 // Write final (per-FPGA) sum back to param block
	 send_wr_req (rg_param_block_addr, param_PARTIAL_SUM, rg_partial_sum);
      endseq
      );

   // ----------------------------------------------------------------
   // INTERFACE

   function BC_MC_Client  fn_mkMC_Client (Integer mc);
      return fn_FIFOFs_to_MC_Client (f_reqs [mc], f_rsps [mc],  f_flush_reqs [mc], f_flush_rsps [mc]);
   endfunction

   method Action start (BC_AEId fpga_id, BC_Data param_block_addr) if (fsm.done);
      rg_fpga_id          <= fpga_id;
      rg_param_block_addr <= truncate (param_block_addr) + (extend (fpga_id) << 3);
      fsm.start;
   endmethod

   method Action waitTillDone() if (fsm.done);
      noAction;
   endmethod

   interface mc_ifcs = genWith (fn_mkMC_Client);
endmodule: mkApp_HW2

// ================================================================
// The following module is instantiated 16 times
//     once for each MC port
// It performs the vector-add for the {even/odd} words of the slices of
// the vectors that are in MC 'chan'

// Rather than continuously reading pairs of input words and writing
// output words, this code alternates 'runs' of 1536 read-pairs
// followed by 1536 writes.  This is purely a Convey-specific
// performance feature, where the memory infrastructure prefers
// consecutive reads or writes, since there is an overhead for
// switching some infrastructure logic between read and write mode.
// Performance measurements show that the vector-add goes 40% slower
// with arbitrarily interleaved reads and writes, compared to this
// solution.

// Performance measurements show that with this solution, this BSV
// vector-add code is as fast (sometimes faster) than the
// native-Verilog vector-add example supplied in Convey's PDK,
// when measured on vector sizes of 100, 1000, 10000, ... 1 Billion.

interface ChanVadd_IFC;
   method Action start (BC_AEId aeid, BC_MC chan,                    // for $displays only
			Bool  evenNotOdd,
			BC_Addr rd_base_1,  BC_Addr rd_limit_1,
			BC_Addr rd_base_2,  BC_Addr rd_limit_2,
			BC_Addr wr_base,    BC_Addr wr_limit);
   method ActionValue #(BC_Data) result ();    // chan partial sum

   interface BC_MC_Client  mc_client;
endinterface

(* synthesize *)
module mkChanVadd (ChanVadd_IFC);
   // These are only needed for $displays
   Reg #(BC_AEId)  rg_aeid <- mkRegU;
   Reg #(BC_MC)    rg_chan <- mkRegU;

   // This reg accumulates the partial sum for this channel
   Reg #(BC_Data)         rg_partial_sum <- mkRegU;

   // The purpose of rg_run_state is to disable the put_rsp method while the
   // parent is still reading the initial params from mem; i.e., so that this method
   // doesn't compete for read responses until we've started the actual computation.
   Reg #(Bit #(2))        rg_run_state   <- mkReg (0);

   Reg #(Bool)            rg_evenNotOdd  <- mkRegU;    // Even or odd words?

   // Input vector 1
   Reg #(BC_Addr)         rg_rd_base_1   <- mkRegU;
   Reg #(BC_Addr)         rg_rd_limit_1  <- mkRegU;

   // Input vector 2
   Reg #(BC_Addr)         rg_rd_base_2   <- mkRegU;
   Reg #(BC_Addr)         rg_rd_limit_2  <- mkRegU;
   Reg #(Bool)            rg_rd_active_2 <- mkReg (False);

   // When rg_reading is True, we do a run of 1536 reads from vin1 and vin2
   // When False, we do a run of 1536 writes to vout
   Reg #(Bool)            rg_reading     <- mkRegU;

   // Read sequencing
   Reg #(Bit #(16))       rg_rd_pc       <- mkRegU;
   Reg #(Bool)            rg_vin1_turn   <- mkRegU;

   // Output vector
   Reg #(BC_Addr)         rg_wr_base     <- mkRegU;
   Reg #(BC_Addr)         rg_wr_limit    <- mkRegU;
   Reg #(Bool)            rg_wr_active   <- mkReg (False);

   // Write sequencing
   Reg #(Bit #(16))       rg_wr_pc       <- mkRegU;

   // Mem req/rsp fifos
   FIFOF #(BC_MC_REQ)     fifo_reqs       <- mkFIFOF;
   FIFOF #(BC_Data)       fifo_rd_rsps    <- mkFIFOF;

   // This reg buffers the J'th response from vector 1 while we're
   // waiting for the J'th response from vector 2
   Reg #(BC_Data)         rg_x1          <- mkRegU;
   Reg #(Bool)            rg_x1_valid    <- mkRegU;

   // Buffer to hold 1536 results, since we alternate 1536x2 reads and 1536 writes
   // We use 'noReset' to avoid load on the Reset tree
   FIFOF #(BC_Data) fifo_sums <- mkSizedBRAMFIFOF (1536, reset_by noReset);

   // ----------------------------------------------------------------
   // Address increments
   // This channel accesses addresses ending in:
   // (if even) A+0x00, +0x10, +0x20, +0x30; then skips to A+0x200
   // (if odd)  A+0x08, +0x18, +0x28, +0x38; then skips to A+0x200+0x08
   // The following logic performs exactly these increments.
   // Conceptually, we increment addr [5:4] and if there's a carry,
   // we increment addr [47:9].

   function BC_Addr addr_incr (BC_Addr addr);
      Bit #(41) x = { addr [47:9], addr [5:4] };
      Bit #(41) y = x + 1;
      BC_Addr new_addr = { y [40:2], addr [8:6], y [1:0], addr [3:0] };
      return new_addr;
   endfunction

   // ----------------------------------------------------------------
   // This rule moves some initializations out of the 'start' method,
   // isolating them from external signals.
   // The main motivation is 'fifo_sums.clear'; when it was in the
   // 'start' method, it had timing closure issues due to high fanout
   // into the 'cache' that is inside the BRAM fifo.

   rule rl_init (rg_run_state == 1);
      fifo_sums.clear;

      rg_reading     <= True;
      rg_rd_pc       <= 0;
      rg_vin1_turn   <= True;     // Alternates reads from vin1 and vin2
      rg_wr_pc       <= 0;
      rg_partial_sum <= 0;
      rg_x1_valid    <= False;
      rg_run_state   <= 2;
   endrule

   // ----------------------------------------------------------------
   // Read generator
   // Alternates these two rules, generating reads from vin1 and vin2
   // Sets rg_reading <= False after issuing 1536 loads from each vector

   rule rl_rd_gen_0 (rg_reading && rg_rd_active_2 && rg_vin1_turn);
      let req  = BC_MC_REQ {cmd_sub: REQ_RD, rtnctl: 0, len: BC_8B,
			    vadr: rg_rd_base_1, data: ?};

      let next_base  = addr_incr (rg_rd_base_1);
      rg_rd_base_1 <= next_base;
      fifo_reqs.enq (req);

      rg_vin1_turn <= False;
      rg_wr_pc <= 0;
   endrule

   rule rl_rd_gen_1 (! rg_vin1_turn);
      let req = BC_MC_REQ {cmd_sub: REQ_RD, rtnctl: 0, len: BC_8B,
			   vadr: rg_rd_base_2, data: ?};

      let next_base = addr_incr (rg_rd_base_2);
      rg_rd_base_2 <= next_base;
      fifo_reqs.enq (req);

      rg_vin1_turn <= True;

      Bool rd_active_2 = (next_base < rg_rd_limit_2);
      rg_rd_active_2 <= rd_active_2;

      if ((! rd_active_2) || (rg_rd_pc == 1535))
	 rg_reading <= False;

      rg_rd_pc <= rg_rd_pc + 1;
   endrule

   // ----------------------------------------------------------------
   // Receive a 64b word from each of vin1 and vin2, write sum to buffer,
   // and accumulate into partial sum
   // Alternates between these rules, receiving data from vin1 and vin2, resply.

   rule rl_get_x1 (! rg_x1_valid);
      rg_x1       <= fifo_rd_rsps.first; fifo_rd_rsps.deq;    // from vin1
      rg_x1_valid <= True;
   endrule

   rule rl_get_x2_sum_and_gen_wr_reqs (rg_x1_valid);
      let x1 = rg_x1;
      let x2 = fifo_rd_rsps.first;  fifo_rd_rsps.deq;    // from vin2
      let x3 = x1 + x2;

      rg_x1_valid <= False;

      // Write x3 to vout.
      fifo_sums.enq (x3);

      // Accumulate x3 in partial sum
      rg_partial_sum <= rg_partial_sum + x3;
   endrule

   // ----------------------------------------------------------------
   // Write buffered sums to mem
   
   rule rg_wr_gen ((! rg_reading) && rg_wr_active);
      // Write x3 to vout.
      let x3 = fifo_sums.first; fifo_sums.deq;

      let req  = BC_MC_REQ {cmd_sub: REQ_WR, rtnctl: 0, len: BC_8B,
			    vadr: rg_wr_base, data: x3};
      fifo_reqs.enq (req);
      
      // Calculate next write address
      let next_base  = addr_incr (rg_wr_base);
      rg_wr_base <= next_base;
      Bool wr_active = (next_base < rg_wr_limit);
      rg_wr_active <= wr_active;

      rg_rd_pc <= 0;

      if ((! wr_active) || (rg_wr_pc == 1535))
	 rg_reading <= True;

      rg_wr_pc <= rg_wr_pc + 1;
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   method Action start (BC_AEId aeid, BC_MC chan,                    // for $displays only
			Bool  evenNotOdd,
			BC_Addr rd_base_1,  BC_Addr rd_limit_1,
			BC_Addr rd_base_2,  BC_Addr rd_limit_2,
			BC_Addr wr_base,    BC_Addr wr_limit) if (rg_run_state == 0);
      rg_aeid        <= aeid;
      rg_chan        <= chan;

      rg_evenNotOdd  <= evenNotOdd;

      rg_rd_base_1   <= rd_base_1;
      rg_rd_limit_1  <= rd_limit_1;

      rg_rd_base_2   <= rd_base_2;
      rg_rd_limit_2  <= rd_limit_2;
      rg_rd_active_2 <= (rd_base_2 < rd_limit_2);

      rg_wr_base     <= wr_base;
      rg_wr_limit    <= wr_limit;
      rg_wr_active   <= (wr_base < wr_limit);

      rg_run_state   <= 1;
   endmethod

   method ActionValue #(BC_Data) result () if ((rg_run_state == 2) && (! rg_wr_active));
      rg_run_state <= 0;
      return rg_partial_sum;
   endmethod

   interface BC_MC_Client mc_client;
      interface Client req_rsp;
	 interface Get request = toGet (fifo_reqs);
	 interface Put response;
	    method Action put (BC_MC_RSP rsp) if (rg_run_state == 2);
	       case (rsp.cmd)
		  RSP_RD_DATA: fifo_rd_rsps.enq (rsp.data);
		  RSP_WR_CMP : noAction; // just discard write-responses
		  default: begin
			      $display ("INTERNAL ERROR: mem response is not RD_DATA or WR_CMP: ", fshow (rsp));
			      $finish (1);
			   end
	       endcase
	    endmethod
	 endinterface
      endinterface
      interface Client flush = clientstub;
   endinterface
endmodule: mkChanVadd

// ================================================================

endpackage
