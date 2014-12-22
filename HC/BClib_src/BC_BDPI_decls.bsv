// Copyright (c) 2012-2015 Bluespec, Inc.  All Rights Reserved.
// Distributed under license.

// Author: Rishiyur S. Nikhil

package BC_BDPI_decls;

// Declarations of imported C functions for hybrid applications
// for Convey platforms.
// (C running on a host, BSV running on attached FPGAs).

// ================================================================
// Starting, waiting for and stopping the App_SW code (C function)

// ----------------------------------------------------------------
// Called by Bluesim testbench at start of BSV program, to start the C app thread bc_SW_main()
//     extern void bc_create_C_thread (void);
import "BDPI" function Action bc_create_C_thread ();

// Called by Bluesim testbench at end of BSV program to wait for C app thread to finish
//     extern void bc_join_C_thread (void);
import "BDPI" function Action bc_join_C_thread ();

// Called by Bluesim testbench to abort the C app thread, if necessary
//     extern void bc_cancel_C_thread (void);
import "BDPI" function Action bc_cancel_C_thread ();

// ----------------------------------------------------------------
// Memory barrier/fence and thread yield function
		 
// Memory barrier/fence:
// If you want two writes W1 and W2 from one thread to be observed in
// that order by another thread, then the first thread should perform
// W1, memory_fence, W2
//     extern void  bc_memory_barrier (void);
import "BDPI" function Action bc_memory_barrier ();

function Action memory_barrier ();
   action
      if (genC) bc_memory_barrier ();
   endaction
endfunction

// For Bluesim thread to yield to SW-side pthread(s)
//     extern void  bc_sched_yield (void);
import "BDPI" function Action bc_sched_yield ();

function Action sched_yield ();
   action
      if (genC) bc_sched_yield ();
   endaction
endfunction

// ----------------------------------------------------------------
// Data communication

//     extern uint64_t  bc_recv_C_to_BSV (void);
import "BDPI" function ActionValue #(Bit #(64))  bc_recv_C_to_BSV ();
//     extern void      bc_send_BSV_to_C (uint64_t x);
import "BDPI" function Action                    bc_send_BSV_to_C (Bit #(64) x);

// ----------------------------------------------------------------
// C memory access (used by the BSV memory model)

//     extern uint64_t  bc_c_mem_read (uint64_t addr);
import "BDPI" function ActionValue #(Bit #(64))  bc_c_mem_read  (Bit #(64) addr);

//     extern void      bc_c_mem_write (uint64_t sz, void * addr, uint64_t x);
import "BDPI" function Action  bc_c_mem_write (Bit #(64) sz, Bit #(64) addr, Bit #(64) x);
		 
// ================================================================

endpackage
