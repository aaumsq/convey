// Copyright (c) 2011-2015 Bluespec, Inc.  All Rights Reserved.
// Distributed under license.

// Author: Rishiyur S. Nikhil

// This package defines a module that encapsulates the dispatch
// interface for one AE (Application Engine) FPGA on the
// Convey HC-1 co-processor.

// The adapter converts the Convey signal-level interface into BSV
// guarded FIFO interfaces.

package BC_Dispatch_Adapter;

// ================================================================

// BSV library imports
import FIFOF         :: *;
// import FIFOLevel :: *;    // TODO: DELETE
import GetPut        :: *;
import FShow         :: *;
import SpecialFIFOs2 :: *;

// Local imports
import BC_Common  :: *;
import BC_HW_IFC  :: *;

// ================================================================
// Signal interface to connect to Convey Verilog

interface BC_Dispatch_Signal_IFC;
   (* always_ready, enable="cae_inst_vld", prefix="" *)
   method  Action  cae_inst_and_data (BC_Inst cae_inst, BC_Data cae_data);

   (* always_ready *)
   method  Bool cae_stall;

   (* always_ready *)
   method  Bool cae_idle;

   (* always_ready *)
   method  BC_ExceptionVec cae_exception;

   (* always_ready *)
   method  BC_Aeg_Cnt_Rep  cae_aeg_cnt;

   (* ready="cae_ret_data_vld" *)
   method  BC_Data cae_ret_data;
endinterface

// ================================================================

module mkBC_Dispatch_Adapter #(BC_Dispatch_IFC  bsv_ifc)
                             (BC_Dispatch_Signal_IFC);

   // ----------------------------------------------------------------
   // FIFO for incoming instruction-and-data
   // Size >= 3 because we can receive upto 1 additional item after asserting 'stall'
   // (see PDK Ref Man v5.1, #9.3.2.7)

   // FIFOLevelIfc #(BC_Dispatch_inst, 3) fifo_insts <- mkGFIFOLevel (ugenq, (! ugdeq), ugcount);
   Bit #(1) dummy_size = ?;
   Bit #(2) dummy_headroom = ?;
   FIFOF #(BC_Dispatch_inst) fifo_insts <- mkSizedFIFOF_AlmostFull (dummy_size,
								    dummy_headroom,
								    ugenq,
								    (! ugdeq));
   
   rule rl_move_insts;
      let x = fifo_insts.first;
      bsv_ifc.put_inst.put (x); fifo_insts.deq;
      // $display ("Dispatch_Adapter: rl_move_insts ", fshow (x));
   endrule

   // ----------------------------------------------------------------
   // FIFO for outgoing ret_data
   Wire #(BC_Dispatch_ret_data)  wi_ret_data       <- mkDWire (?);
   Wire #(Bool)                  wi_ret_data_valid <- mkDWire (False);

   rule rl_move_ret_data;
      let x <- bsv_ifc.get_ret_data.get;
      wi_ret_data       <= x;
      wi_ret_data_valid <= True;
   endrule

   // ----------------------------------------------------------------
   // Exceptions
   Wire #(BC_ExceptionVec)  wi_exception <- mkDWire (0);

   rule rl_move_exception;
      let x <- bsv_ifc.get_exception.get;
      wi_exception <= x;
   endrule

   // ----------------------------------------------------------------
   // Interface

   method Action cae_inst_and_data (BC_Inst cae_inst, BC_Data cae_data);
      let x = BC_Dispatch_inst { inst: cae_inst, data: cae_data };
      fifo_insts.enq (x);
      // $display ("Dispatch_Adapter: received instruction ", fshow (x));
   endmethod

   // OLD: method Bool cae_stall = fifo_insts.isGreaterThan (1);    // stall when 1 space left out of 3
   method Bool cae_stall = (! fifo_insts.notFull);

   method Bool             cae_idle      = bsv_ifc.idle;
   method BC_ExceptionVec  cae_exception = wi_exception;
   method BC_Aeg_Cnt_Rep   cae_aeg_cnt   = bsv_ifc.aeg_cnt;

   method BC_Data cae_ret_data () if (wi_ret_data_valid);
      return wi_ret_data;
   endmethod
endmodule

// ================================================================

endpackage
