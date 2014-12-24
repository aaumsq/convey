// Copyright (c) 2012-2015 Bluespec, Inc.  All Rights Reserved
// Distributed under license.

// Author: Rishiyur S. Nikhil

// This is a C runtime library for C-BSV hybrid applications
// for Convey platforms.
// (C running on a host, BSV running on attached FPGAs).
//
// Two .o files are provided:
//    - BC_linkage_Bluesim.o
//    - BC_linkage_HW.o
//
// Rename one of these to: BC_linkage.o
// (or make a symbolic link with this name to one of the .o's)
// ================================================================

#ifndef BC_LINKAGE_H
#define BC_LINKAGE_H

// ================================================================

#ifdef FOR_HW

#warning "INFO: Compiling for Convey hardware (or Convey PDK sim)"

#elif defined (FOR_SIM)

#warning "INFO: Compiling for Bluesim"

#else

#error "FOR_HW and FOR_SIM undefined; please define one of them"

#endif

// ****************************************************************
// Assumed external name for C app's top-level function (entry point)

extern "C" void * bc_SW_main (void *args);

// ================================================================
// On Convey HW, there is 'host-side' memory and 'coprocessor-side' memory,
// and Convey provides library routines to malloc on coproc side,
// copy data back and forth, etc.
// When running for simulation only (not on HW), we revert these to
// ordinary malloc, memcpy, etc.

#ifndef FOR_HW

#define  cny_cp_malloc          malloc
#define  cny_cp_memcpy          memcpy
#define  cny_cp_posix_memalign  posix_memalign

#endif

// ================================================================
// Convenience defs

typedef enum { False, True } Bool;

// For converting bits to/from uint64_t
// To avoid alignment issues, we typically use uint64_t for all
// args/results communicated between host and FPGA

extern "C" uint64_t  ptr_to_ui64    (void *p);
extern "C" void *    ui64_to_ptr    (uint64_t x);
extern "C" uint64_t  double_to_ui64 (double x);
extern "C" double    ui64_to_double (uint64_t x);

// ================================================================
// The following routine initializes the HW side of the hybrid platform

extern "C" void  bc_init_HW (char *personality_name);

// ================================================================
// This routine performs the call/return to the HW function.
// The argument is typically a pointer to a parameter block in memory.
// The result is the exception vector from the HW function (normally all zeros).

extern "C" uint64_t  bc_call_HW (uint64_t  x);

// ================================================================
// The following mallocs either on the host or the FPGA side.
// If running in Bluesim, it just does a normal malloc.
// 'alignment' specifies how many low bits of the address must be 0.
// The allocated region will grown by some amount to accomplish this aligment.
// The original unaligned pointer is returned in 'free_this_ptr'
// (if it is not NULL) and that is the pointer that should be used
// to free the memory in a later free() call.
// If exit_on_failure is True, calls exit(1) on failure to malloc
// else returns NULL on failure (just like malloc).

extern "C" void *bc_malloc (const char   *reason,
			size_t        size,
			unsigned int  alignment,
			Bool          fpga_side,
			void        **free_this_ptr,
			Bool          exit_on_failure);

// ================================================================
// Memory barrier/fence:
// If you want two writes W1 and W2 from one thread to be observed in
// that order by another thread, then the first thread should perform
// W1, memory_fence, W2

extern "C" void  bc_memory_barrier (void);

// If the user thread is in an idle loop, it is good to yield the processor

extern "C" void  bc_sched_yield (void);

// ****************************************************************
// Reading and writing C memory from BSV, in simulation

extern "C" uint64_t bc_c_mem_read    (uint64_t len, uint64_t addr);
extern "C" void     bc_c_mem_write   (uint64_t len, uint64_t addr, uint64_t x);
extern "C" uint64_t bc_c_mem_atomic  (uint64_t cmd_sub, uint64_t len, uint64_t addr, uint64_t x64u);

// ****************************************************************

#endif
