// Copyright (c) 2012-2015 Bluespec, Inc.  All Rights Reserved.
// Distributed under license.

// Author: Rishiyur S. Nikhil

// This package defines a module that adapts the Convey HC-1 co-processor
// FPGA Management interface, between BSV to Convey Verilog signals

package BC_Management_Adapter;

// ================================================================

// BSV library imports
import FIFOF     :: *;
import FIFOLevel :: *;
import GetPut    :: *;

// Local imports
import BC_Common  :: *;
import BC_HW_IFC  :: *;

// ================================================================
// Signal interface

interface BC_Management_Signal_IFC;
   // Ring in
   (* always_ready, always_enabled, prefix="" *)
   method Action m_ring_ctl_in (BC_RingCtl  cae_ring_ctl_in);
   (* always_ready, always_enabled, prefix="" *)
   method Action m_ring_data_in (BC_RingData cae_ring_data_in);

   // Ring out
   (* always_ready *)
   method BC_RingCtl  cae_ring_ctl_out;
   (* always_ready *)
   method BC_RingData cae_ring_data_out;

   // 31_31_interleave boot status
   (* always_ready, always_enabled, prefix="" *)
   method Action m_csr_31_31_intlv_dis (Bool csr_31_31_intlv_dis);
endinterface

// ================================================================

module mkBC_Management_adapter
   #(BC_Management_IFC bsv_ifc)
   (BC_Management_Signal_IFC);

   Wire #(BC_RingCtl)  wi_ring_ctl_in    <- mkBypassWire;
   Wire #(BC_RingData) wi_ring_data_in   <- mkBypassWire;

   Wire #(BC_RingCtl)  wi_ring_ctl_out   <- mkBypassWire;
   Wire #(BC_RingData) wi_ring_data_out  <- mkBypassWire;

   (* fire_when_enabled, no_implicit_conditions *)    // i.e., fire on every clock
   rule rl_always_put;
      bsv_ifc.put_ring_msg_in.put (BC_Ring_msg {ctl: wi_ring_ctl_in, data: wi_ring_data_in});
   endrule

   (* fire_when_enabled, no_implicit_conditions *)    // i.e., fire on every clock
   rule rl_always_get;
      let x <- bsv_ifc.get_ring_msg_out.get;
      wi_ring_ctl_out  <= x.ctl;
      wi_ring_data_out <= x.data;
   endrule

   // ----------------------------------------------------------------
   // Interface

   method Action m_ring_ctl_in  (BC_RingCtl  cae_ring_ctl_in)  = action wi_ring_ctl_in  <= cae_ring_ctl_in;  endaction;
   method Action m_ring_data_in (BC_RingData cae_ring_data_in) = action wi_ring_data_in <= cae_ring_data_in; endaction;

   method BC_RingCtl  cae_ring_ctl_out  = wi_ring_ctl_out;
   method BC_RingData cae_ring_data_out = wi_ring_data_out;

   method Action m_csr_31_31_intlv_dis (Bool csr_31_31_intlv_dis) =
      action  bsv_ifc.m_csr_31_31_intlv_dis (csr_31_31_intlv_dis);  endaction;
endmodule

// ================================================================

endpackage
