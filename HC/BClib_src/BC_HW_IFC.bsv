// Copyright (c) 2012-2015 Bluespec, Inc.  All Rights Reserved.
// Distributed under license.

// Author: Rishiyur S. Nikhil

// NOTE: This package is for the Convey HC platform (and not for Convey MX)

// This package defines interface BC_HW_IFC, the normal top-level BSV
// interface of the BSV App, i.e., the interface of the user's
// top-level BSV app module.

package BC_HW_IFC;

// ================================================================
// BSV library imports

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import Vector       :: *;

// BClib imports

import BC_Utils :: *;

// ================================================================
// Top-level BSV interface for App

interface BC_HW_IFC;
   // Memory interfaces
   interface Vector #(8, BC_MC_Client_Pair)  mc_ifcs;

   interface BC_Dispatch_IFC    dispatch_ifc;
   interface BC_Management_IFC  management_ifc;
endinterface

typedef  4  BC_Num_FPGAs;
Integer  bc_num_FPGAs = valueOf (BC_Num_FPGAs);

// ================================================================
// MC interface (Memory Controllers/Channels)
// These defs are common for all 16 actual ports (8 channels x even/odd)

typedef Bit #(48)  BC_Addr;
typedef Bit #(64)  BC_Data;

typedef Bit #(32)  BC_RdCtl;

// ----------------
// Transfer sizes (1, 2, 4 or 8 Bytes)

typedef  enum { BC_1B, BC_2B, BC_4B, BC_8B } BC_DataSize
   deriving (Eq, Bits, FShow);

// ----------------
// Mem read requests

typedef struct {
   BC_DataSize  size;
   BC_Addr      addr;
   BC_RdCtl     rdctl;
   } BC_MC_rd_req
   deriving (Bits, FShow);

// ----------------
// Mem write requests

typedef struct {
   BC_DataSize  size;
   BC_Addr      addr;
   BC_Data      data;
   } BC_MC_wr_req
   deriving (Bits, FShow);

// ----------------
// Mem read responses (writes have no response)

typedef struct {
   BC_RdCtl  rdctl;
   BC_Data   data;
   } BC_MC_rd_rsp
   deriving (Bits, FShow);

// ----------------------------------------------------------------
// Flush requests and responses
// TODO: Restore to Bit #(0) after internal compiler error on Bit #(0) is fixed

// typedef Bit #(1)   BC_MC_flush_req;
// typedef Bit #(1)   BC_MC_flush_rsp;

typedef struct {
   Bit #(1) dummy;
   } BC_MC_flush_req
deriving (Bits);

typedef struct {
   Bit #(1) dummy;
   } BC_MC_flush_rsp
deriving (Bits);

BC_MC_flush_req  bc_mc_flush_req = ?;
BC_MC_flush_rsp  bc_mc_flush_rsp = ?;

// ================================================================
// Type 0 Memory Interfaces
// Separate read and write request streams,
// read-response stream, and
// and there is no write-response stream.

interface BC_MC_Client;
   interface Get #(BC_MC_rd_req)     get_rd_req;
   interface Get #(BC_MC_wr_req)     get_wr_req;
   interface Put #(BC_MC_rd_rsp)     put_rd_rsp;

   interface Get #(BC_MC_flush_req)  get_flush_req;
   interface Put #(BC_MC_flush_rsp)  put_flush_rsp;
endinterface

interface BC_MC_Server;
   interface Put #(BC_MC_rd_req)     put_rd_req;
   interface Put #(BC_MC_wr_req)     put_wr_req;
   interface Get #(BC_MC_rd_rsp)     get_rd_rsp;

   interface Put #(BC_MC_flush_req)  put_flush_req;
   interface Get #(BC_MC_flush_rsp)  get_flush_rsp;
endinterface

function BC_MC_Client  fn_FIFOFs_to_MC_Client (FIFOF #(BC_MC_rd_req)     f_rd_reqs,
					       FIFOF #(BC_MC_wr_req)     f_wr_reqs,
					       FIFOF #(BC_MC_rd_rsp)     f_rd_rsps,
					       FIFOF #(BC_MC_flush_req)  f_flush_reqs,
					       FIFOF #(BC_MC_flush_rsp)  f_flush_rsps);
   return interface BC_MC_Client;
	     interface get_rd_req    = toGet (f_rd_reqs);
	     interface get_wr_req    = toGet (f_wr_reqs);
	     interface put_rd_rsp    = toPut (f_rd_rsps);
	     interface get_flush_req = toGet (f_flush_reqs);
	     interface put_flush_rsp = toPut (f_flush_rsps);
	  endinterface;
endfunction

function BC_MC_Server  fn_FIFOFs_to_MC_Server (FIFOF #(BC_MC_rd_req)     f_rd_reqs,
					       FIFOF #(BC_MC_wr_req)     f_wr_reqs,
					       FIFOF #(BC_MC_rd_rsp)     f_rd_rsps,
					       FIFOF #(BC_MC_flush_req)  f_flush_reqs,
					       FIFOF #(BC_MC_flush_rsp)  f_flush_rsps);
   return interface BC_MC_Server;
	     interface put_rd_req    = toPut (f_rd_reqs);
	     interface put_wr_req    = toPut (f_wr_reqs);
	     interface get_rd_rsp    = toGet (f_rd_rsps);
	     interface put_flush_req = toPut (f_flush_reqs);
	     interface get_flush_rsp = toGet (f_flush_rsps);
	  endinterface;
endfunction

instance Connectable #(BC_MC_Client, BC_MC_Server);
   module mkConnection #(BC_MC_Client c, BC_MC_Server s) (Empty);
      mkConnection (c.get_rd_req,    s.put_rd_req);
      mkConnection (c.get_wr_req,    s.put_wr_req);
      mkConnection (c.put_rd_rsp,    s.get_rd_rsp);
      mkConnection (c.get_flush_req, s.put_flush_req);
      mkConnection (c.put_flush_rsp, s.get_flush_rsp);
   endmodule
endinstance

instance Connectable #(BC_MC_Server, BC_MC_Client);
   module mkConnection #(BC_MC_Server s, BC_MC_Client c) (Empty);
      mkConnection (c, s);
   endmodule
endinstance

// ----------------
// Even/odd MC port pairs (Type 0)

typedef  Tuple2 #(BC_MC_Client, BC_MC_Client)  BC_MC_Client_Pair;
typedef  Tuple2 #(BC_MC_Server, BC_MC_Server)  BC_MC_Server_Pair;

// ----------------------------------------------------------------
// Utility functions for address calculations

typedef Bit #(3)  BC_MC;    // 0..7 (index for 8 memory channels)

// ----------------
// Return the MC for an address

function BC_MC bc_mc_of_addr (BC_Addr addr) = addr [8:6];

// ----------------
// Address alignments and offsets
// A 'chunk'  is a set of contigous addresses within an MC (64 B)
// A 'stripe' is a set of continous addresses across 8 MCs (512 B)

function Bool bc_is_aligned_to_1B              (BC_Addr addr) = True;
function Bool bc_is_aligned_to_2B              (BC_Addr addr) = (addr [0] == 0);
function Bool bc_is_aligned_to_4B              (BC_Addr addr) = (addr [1:0] == 0);
function Bool bc_is_aligned_to_8B              (BC_Addr addr) = (addr [2:0] == 0);
function Bool bc_is_aligned_to_stripe_MC_chunk (BC_Addr addr) = (addr [5:0] == 0);
function Bool bc_is_aligned_to_stripe          (BC_Addr addr) = (addr [8:0] == 0);

function BC_Addr  bc_offset_in_stripe_MC_chunk (BC_Addr addr) = extend (addr [5:0]);
function BC_Addr  bc_offset_in_stripe          (BC_Addr addr) = extend (addr [8:0]);

// ----------------
// Given a vec_base_addr of a vector,
// returns the base address for channel 'chanJ' (in 0..7),
// which is either vec_base_addr itself (if it is in chanJ)
// or the first address in chanJ that is above vec_base_addr

function BC_Addr bc_base_addr_for_chan (Integer chanJ, BC_Addr vec_base_addr);
   BC_MC  chanJ_b3 = fromInteger (chanJ);
   BC_MC  chan_of_base = bc_mc_of_addr (vec_base_addr);
   Bit#(39) addr_bits_above_chan = vec_base_addr[47:9];
   Bit#(6)  addr_bits_below_chan = vec_base_addr[5:0];
   Bit#(39) upper = addr_bits_above_chan + ((chanJ_b3 < chan_of_base) ? 39'd1 : 39'd0);
   Bit#(6)  lower = (chanJ_b3 == chan_of_base) ? addr_bits_below_chan : 6'd0;
   return { upper, chanJ_b3, lower };
endfunction

// Note: the following version of the same function as above triggers
// a Xilinx parser bug which results in wrong gate-level Verilog generation
// which, in turn, can trigger a hardware error on the Convey HC machine.
// Briefly: for addresses directed at MC 4, the ISE-produced gates wrongly
// sets the MC bits [8:6] to zero. When this address is on the x86 side, it
// gets sent there. When the response arrives, because the MC bits were zero,
// the response gets sent back to MC 0 instead which, of course, is not expecting
// any such response, and so raises a hardware exception.  This Xilinx ISE bug
// has been reported to Xilinx, and they identified it as a bug in pre ISE 14.2
// parsers.  They suggest using 14.2, and setting a use-new-parser flag, as a
// workaround.  However, the above BSV recoding produces slightly different
// Verilog which, fortunately, does not trigger the ISE parser bug.
// TODO: Delete this after everyone has moved to ISE 14.2 or beyond

// function BC_Addr bc_base_addr_for_chan (Integer chanJ, BC_Addr vec_base_addr);
//    BC_MC  chanJ_b3 = fromInteger (chanJ);
//    BC_MC  chan_of_base = bc_mc_of_addr (vec_base_addr);
//    Bit #(39) addr_bits_above_chan = vec_base_addr [47:9];
//    return (  (chanJ_b3 < chan_of_base) ?      { (addr_bits_above_chan + 1), chanJ_b3, 6'h0 }
// 	   : (  (chanJ_b3 == chan_of_base) ?  vec_base_addr
// 	      :                               { addr_bits_above_chan, chanJ_b3, 6'h0 } ) );
// endfunction

// ----------------
// Given an 8B-aligned address addr_8B,
// returns address of next 8B-aligned in channel chanJ

function BC_Addr bc_next_addr_8B_in_chan (BC_Addr addr_8B);
   return (  (addr_8B [5:0] != 6'h38)                 // h38 is offset of last quadword in 64B block in chan
	   ? (addr_8B + 8)
	   : ( { (addr_8B + 512)[47:6], 6'h0 } ) );   // skip to next chunk in this channel
endfunction

// ================================================================
// Dispatch interface

typedef  Bit #(18)  BC_Aeg_Cnt_Rep;
typedef  Bit #(6)   BC_Aeg_Idx;
typedef  Bit #(16)  BC_ExceptionVec;

// Standard exceptions

BC_ExceptionVec bc_exception_unimplemented_inst = 16'h0001;    // Bit 0, aka AEUIE
BC_ExceptionVec bc_exception_invalid_aeg_idx    = 16'h0002;    // Bit 1, aka AEERE (Element Range Exception)

typedef  Bit #(32)  BC_Inst;

// Dispatch instructions, as seen by BSV app
typedef struct {
   BC_Inst  inst;
   BC_Data  data;
   } BC_Dispatch_inst
   deriving (Bits);

instance FShow #(BC_Dispatch_inst);
   function Fmt fshow (BC_Dispatch_inst  inst_and_data);
      return $format ("Dispatch_inst {inst:%8h, data:%16h}", inst_and_data.inst, inst_and_data.data);
   endfunction
endinstance

// Dispatch ret_data, as seen by BSV app
typedef BC_Data  BC_Dispatch_ret_data;

// The BSV Dispatch interface

interface BC_Dispatch_IFC;
   interface Put #(BC_Dispatch_inst)      put_inst;
   interface Get #(BC_Dispatch_ret_data)  get_ret_data;
   interface Get #(BC_ExceptionVec)       get_exception;

   (* always_ready *)
   method    Bool                         idle;
   (* always_ready *)
   method    BC_Aeg_Cnt_Rep               aeg_cnt;
endinterface

// ================================================================
// Management interface

typedef Bit #(2) BC_AEId;

interface BC_Management_IFC;
   (* always_ready, always_enabled *)
   method Action set_aeid (BC_AEId aeid);

   (* always_ready, always_enabled *)
   method Action m_csr_31_31_intlv_dis (Bool csr_31_31_intlv_dis);

   // ----------------
   // CSR ring
   (* always_ready, always_enabled *)
   interface Put #(BC_Ring_msg) put_ring_msg_in;

   (* always_ready, always_enabled *)
   interface Get #(BC_Ring_msg) get_ring_msg_out;
endinterface

typedef Bit #(4)  BC_RingCtl;
typedef Bit #(16) BC_RingData;

typedef struct {
   BC_RingCtl   ctl;
   BC_RingData  data;
   } BC_Ring_msg
   deriving (Bits);

// ================================================================

endpackage
