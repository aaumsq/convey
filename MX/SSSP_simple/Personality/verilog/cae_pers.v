/*****************************************************************************/
//
// Module	   : cae_pers.vpp
// Revision	   :  Revision: 1.3  
// Last Modified On:  Date: Feb 28 2015  
// Last Modified By:  Author: Dan Zhang  
//
//-----------------------------------------------------------------------------
//
// Original Author : gedwards
// Created On      : Wed Oct 10 09:26:08 2007
//
//-----------------------------------------------------------------------------
//
// Description     : Sample PDK Vector Add Personality
//
//                   Top-level of vadd personality.  For a complete list of 
//                   optional ports, see 
//                   /opt/convey/pdk/<rev>/<platform>/doc/cae_pers.v
//
//-----------------------------------------------------------------------------
//
// Copyright (c) 2007-2013 : created by Convey Computer Corp. This model is the
// confidential and proprietary property of Convey Computer Corp.
//
/*****************************************************************************/
/*  Id: cae_pers.vpp,v 1.3 2013-01-23 17:37:19 gedwards Exp   */

`timescale 1 ns / 1 ps

`include "pdk_fpga_defines.vh"

(* keep_hierarchy = "true" *)
module cae_pers #(parameter    NUM_MC_PORTS = 16) (
   //
   // Clocks and Resets
   //
   input		clk,		// 150MHz PDK core clock
   input		clk_csr,	// half-rate (75MHz) clock for CSR chain
   input		clk2x,		// 2x rate (300MHz) clock
   input		i_reset,	// global reset synchronized to 150MHz clock
   input		i_csr_reset_n,	// 75MHz active-low reset for CSR chain

   // Signals for async personality clock
   input		ppll_reset,
   output		ppll_locked,
   output		clk_per,

   //
   // Dispatch Interface
   //
   input  [31:0]	cae_inst,
   input  [63:0]	cae_data,
   input		cae_inst_vld,

   output [17:0]	cae_aeg_cnt,
   output [15:0]	cae_exception,
   output [63:0]	cae_ret_data,
   output		cae_ret_data_vld,
   output		cae_idle,
   output		cae_stall,

   //
   // MC Interface(s)
   //
   output [NUM_MC_PORTS*1-1 :0]         mc_rq_vld,
   output [NUM_MC_PORTS*32-1:0]         mc_rq_rtnctl,
   output [NUM_MC_PORTS*64-1:0]         mc_rq_data,
   output [NUM_MC_PORTS*48-1:0]         mc_rq_vadr,
   output [NUM_MC_PORTS*2-1 :0]         mc_rq_len,
   output [NUM_MC_PORTS*4-1 :0]         mc_rq_sub,
   output [NUM_MC_PORTS*3-1 :0]         mc_rq_cmd,
   input  [NUM_MC_PORTS*1-1 :0]         mc_rq_stall,
   
   input  [NUM_MC_PORTS*1-1 :0]         mc_rs_vld,
   input  [NUM_MC_PORTS*3-1 :0]         mc_rs_cmd,
   input  [NUM_MC_PORTS*3-1 :0]         mc_rs_sub,
   input  [NUM_MC_PORTS*64-1:0]         mc_rs_data,
   input  [NUM_MC_PORTS*32-1:0]         mc_rs_rtnctl,
   output [NUM_MC_PORTS*1-1 :0]         mc_rs_stall,

   //
   // Write flush 
   //
   output [NUM_MC_PORTS*1-1 :0]         mc_rq_flush,
   input  [NUM_MC_PORTS*1-1 :0]         mc_rs_flush_cmplt,

   //
   // AE-to-AE Interface not used
   //

   //
   // Management/Debug Interface
   //
   input  [3:0]		cae_ring_ctl_in,
   input  [15:0]	cae_ring_data_in,
   output [3:0]		cae_ring_ctl_out,
   output [15:0]	cae_ring_data_out,

   //
   // Miscellaneous
   //
   input  [1:0]		i_aeid,
   input		csr_31_31_intlv_dis
);

`include "pdk_fpga_param.vh"

   //
   // Local clock generation
   //
   (* KEEP = "true" *) wire reset_per;
   cae_clock clock (
      .clk(clk),
      .clk2x(clk2x),
      .clkhx(clk_csr), 
      .i_reset(i_reset),
      .ppll_reset(ppll_reset),

      .clk_per(clk_per),
      .clk_per_2x(clk_per_2x),
      .ppll_locked(ppll_locked),
      .reset_per(reset_per)
   );


   mkCae_pers pers (.clk(clk_per),
		            .i_reset(i_reset),
                    
		            .clk_csr(),
		            .clk2x(),
		            .i_csr_reset_n(),
		            .ppll_reset(),
		            .ppll_locked(),
		            .i_aeid(i_aeid),
                    
		            .cae_inst(cae_inst),
		            .cae_data(cae_data),
		            .cae_inst_vld(cae_inst_vld),
		            .cae_stall(cae_stall),
		            .cae_idle(cae_idle),
		            .cae_exception(cae_exception),
		            .cae_aeg_cnt(cae_aeg_cnt),
		            .cae_ret_data(cae_ret_data),
		            .cae_ret_data_vld(cae_ret_data_vld),
                    
		            .mc_rq_vld(mc_rq_vld),
		            .mc_rq_cmd(mc_rq_cmd),
		            .mc_rq_sub(mc_rq_sub),
		            .mc_rq_len(mc_rq_len),
		            .mc_rq_vadr(mc_rq_vadr),
		            .mc_rq_rtnctl(mc_rq_rtnctl),
		            .mc_rq_data(mc_rq_data),
		            .mc_rq_stall(mc_rq_stall),
                    
		            .mc_rs_vld(mc_rs_vld),
		            .mc_rs_cmd(mc_rs_cmd),
		            .mc_rs_sub(mc_rs_sub),
		            .mc_rs_rtnctl(mc_rs_rtnctl),
		            .mc_rs_data(mc_rs_data),
		            .mc_rs_stall(mc_rs_stall),
		            .mc_rq_flush(mc_rq_flush),
		            .mc_rs_flush_cmplt(mc_rs_flush_cmplt),
                    
		            .cae_ring_ctl_in(cae_ring_ctl_in),
		            .cae_ring_data_in(cae_ring_data_in),
		            .cae_ring_ctl_out(cae_ring_ctl_out),
		            .cae_ring_data_out(cae_ring_data_out),
		            .csr_31_31_intlv_dis(csr_31_31_intlv_dis),
		            .clk_per(),
		            .CLK_GATE_clk_per(CLK_GATE_clk_per));

endmodule // cae_pers
