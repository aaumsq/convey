// Copyright (c) 2012-2015 Bluespec, Inc.  All Rights Reserved
// Distributed under license.

// Author: Rishiyur S. Nikhil

// This is a C runtime library for the Bluespec Convey library.
// (C running on a host, BSV running on attached FPGAs).
//
// Two .o files are provided:
//    - BC_linkage_Bluesim.o    // for building a Bluesim simulation
//    - BC_linkage_HW.o         // for building for Convey HC HW
//
// ================================================================

#include  <stdio.h>
#include  <stdlib.h>
#include  <unistd.h>
#include  <sys/types.h>
#include  <string.h>
#include  <stdint.h>
#include  <pthread.h>
#include  <sched.h>
#include  <assert.h>

// ================================================================
// CONFIG COMPILATION OF THIS FILE

// Two possible configs:
//   FOR_HW          Execution begins at main()
//                       which calls common_main()
//                   Calls Convey 'copcall' to send/receive initial args/final result
//
//   else (FOR_SIM)  Execution begins in BSV SimMain,
//                   which creates a pthread for bc_SW_main()
//                       which calls common_main()
//                   Calls PDKlib C API to send/receive initial arg/final result

// #define FOR_HW

// #define DEBUG             // for printfs
// #define STANDALONE_TEST   // enables 'main' etc., below

// ================================================================

#include  "BC_linkage.h"

// ================================================================

#ifdef FOR_HW

#include <convey/usr/cny_comp.h>
#include <ctype.h>

static cny_image_t        sig2;
static cny_image_t        sig;

// The following 3 functions are implemented in Convey Scalar Co-processor Assembly
// (see file cpAsm.s)

extern void cpWrAEG0 ();
extern void cpCaep ();
extern long cpRdAEG0 ();

#endif

// ****************************************************************
// ****************************************************************
// ****************************************************************
// WARNING: the following are not user-callable routines.
// They are called from the Bluesim testbench code.

static pthread_mutex_t  mutex_for_mb = PTHREAD_MUTEX_INITIALIZER;

// ----------------
// Each "channel" between BSV and C is synchronized with a block like this

typedef struct {
    pthread_mutex_t  mutex;
    pthread_cond_t   cond_notFull;
    pthread_cond_t   cond_notEmpty;
    int              full;
} chan_sync_t;

// ================================================================
// The following are about communicating data between C and Bluesim.
// Specifically, between the pthread that runs the C app code,
// and the pthread in Bluesim that executes BSV code

static
void assert_no_error (char *s1, char *s2, int status)
{
    if (status != 0) {
	fprintf (stderr, "ERROR: %s %s => status %0d\n", s1, s2, status);
	exit (1);
    }
}

// ----------------
// Initialize a channel sync block

static
void init_chan_sync (char *context, chan_sync_t *chan_sync)
{
    int status;

#ifdef DEBUG
    printf ("%s: initializing chan_sync block\n", context);
#endif
    status = pthread_mutex_init (& (chan_sync->mutex), NULL);
    assert_no_error (context, "pthread_mutex_init (mutex)", status);

    status = pthread_cond_init (& (chan_sync->cond_notFull), NULL);
    assert_no_error (context, "pthread_cond_init (notFull)", status);

    status = pthread_cond_init (& (chan_sync->cond_notEmpty), NULL);
    assert_no_error (context, "pthread_cond_init (notEmpty)", status);

    chan_sync->full  = False;
}

// ----------------
// Channel sync functions

static
void send_wait (char *context, chan_sync_t *chan_sync)
{
#ifdef DEBUG
    uint64_t n_tries = 0;
#endif

    pthread_mutex_lock (& chan_sync->mutex);
    while (chan_sync->full) {
#ifdef DEBUG
	if (n_tries == 0)
	    printf ("%s: wait (cond_notFull) waiting\n", context);
	n_tries ++;
#endif
	pthread_cond_wait (& chan_sync->cond_notFull, & chan_sync->mutex);
    }
#ifdef DEBUG
    printf ("%s: wait (cond_notFull) waiting done after %0lld tries\n", context, n_tries);
#endif
}

static
void send_done (char *context, chan_sync_t *chan_sync)
{
    chan_sync->full = True;
    pthread_mutex_unlock (& chan_sync->mutex);
    pthread_cond_signal (& chan_sync->cond_notEmpty);
}

// ----------------

static
void recv_wait (char *context, chan_sync_t *chan_sync)
{
#ifdef DEBUG
    uint64_t n_tries = 0;
#endif

    pthread_mutex_lock (& chan_sync->mutex);
    while (! chan_sync->full) {
#ifdef DEBUG
	if (n_tries == 0)
	    printf ("%s: wait (cond_notEmpty) waiting\n", context);
	n_tries ++;
#endif
	pthread_cond_wait (& chan_sync->cond_notEmpty, & chan_sync->mutex);
    }
#ifdef DEBUG
    printf ("%s: wait (cond_notEmpty) waiting done after %0lld tries\n", context, n_tries);
#endif
}

static
void recv_done (char *context, chan_sync_t *chan_sync)
{
    chan_sync->full = False;
    pthread_mutex_unlock (& chan_sync->mutex);
    pthread_cond_signal (& chan_sync->cond_notFull);
}

// ----------------------------------------------------------------
// C to BSV channel

// ----------------
// Sync block and data buffer

static chan_sync_t  chan_sync_C_to_BSV;
static uint64_t     data_C_to_BSV;

// ----------------
// C call to send one uint64_t to BSV (typically a pointer to a param block)

static
void bc_send_C_to_BSV (uint64_t arg)
{
    send_wait ("bc_send_C_to_BSV", & chan_sync_C_to_BSV);
    data_C_to_BSV = arg;
    send_done ("bc_send_C_to_BSV", & chan_sync_C_to_BSV);
}

// ----------------
// BSV call to receive one uint64_t from C
//     import "BDPI" function ActionValue #(UInt #(64)) bc_recv_C_to_BSV ();

// The following is not 'static' because it has to be visible via an import BDPI
uint64_t bc_recv_C_to_BSV (void)
{
    uint64_t result;

    recv_wait ("bc_recv_C_to_BSV", & chan_sync_C_to_BSV);    // start mutex region
    result = data_C_to_BSV;
    recv_done ("bc_recv_C_to_BSV", & chan_sync_C_to_BSV);    // end mutex region

    return result;
}

// ----------------------------------------------------------------
// BSV to C channel

// ----------------
// Sync block and data buffer

static chan_sync_t  chan_sync_BSV_to_C;
static uint64_t     data_BSV_to_C;

// ----------------
// BSV call to send one 64-bit value from BSV to C
//     import "BDPI" function Action bc_send_BSV_to_C (Bit #(64) vx);

// The following is not 'static' because it has to be visible via an import BDPI
void bc_send_BSV_to_C (uint64_t x)
{
    send_wait ("bc_send_BSV_to_C", & chan_sync_BSV_to_C);    // start mutex region
    data_BSV_to_C = x;
    send_done ("bc_send_BSV_to_C", & chan_sync_BSV_to_C);    // end mutex region
}

// ----------------
// C call to recv one 64-bit value from BSV

static
uint64_t bc_recv_BSV_to_C (void)
{
    uint64_t result;
    recv_wait ("bc_recv_BSV_to_C", & chan_sync_BSV_to_C);
    result = data_BSV_to_C;
    recv_done ("bc_recv_BSV_to_C", & chan_sync_BSV_to_C);
    return result;
}

// ================================================================
// The following section is about creating/waiting/killing the
// C thread corresponding to the C app code

static pthread_t  bc_SW_main_thread_id;

// ----------------
// Call this at start of BSV program, to start C/C++ app thread
//     import "BDPI" function Action bc_create_C_thread ();

void bc_create_C_thread (void)
{
    int status;

#ifdef DEBUG
    printf ("Creating C thread for bc_SW_main and initializing channels\n");
#endif

    status = pthread_mutex_init (& (mutex_for_mb), NULL);
    assert_no_error ("create_C_thread", "pthread_mutex_init (mutex_for_mb)", status);

    // Initialize the channels to send arg and return exception-vector result
    init_chan_sync ("create_C_thread: init chan_sync_C_to_BSV", & chan_sync_C_to_BSV);
    init_chan_sync ("create_C_thread: init chan_sync_BSV_to_C", & chan_sync_BSV_to_C);

    status = pthread_create (& bc_SW_main_thread_id, NULL, bc_SW_main, NULL);
    assert_no_error ("create_C_thread", "pthread_create (bc_SW_main_thread_id, ..., bc_SW_main, ...)", status);
}

// ----------------
// Use this function if the C++ thread is expected to terminate and you want to wait for it
//     import "BDPI" function Action bc_join_C_thread ();

void bc_join_C_thread (void)
{
#ifdef DEBUG
    printf ("Joining C thread for bc_SW_main \n");
#endif
    int status = pthread_join (bc_SW_main_thread_id, NULL);
    assert_no_error ("main", "pthread_join (bc_SW_main_thread_id, ...) for bc_SW_main", status);
}

// ----------------
// Use this function if the C++ thread should just be k.o.'d
//     import "BDPI" function Action bc_cancel_C_thread ();

void bc_cancel_C_thread (void)
{
#ifdef DEBUG
    printf ("Cancelling C thread for bc_SW_main \n");
#endif
    int status = pthread_cancel (bc_SW_main_thread_id);
    assert_no_error ("main", "pthread_join (bc_SW_main_thread_id, ...) for bc_SW_main", status);
}

// ================================================================
// Accessing C memory from from BSV (where BSV has been given a pointer to a C location)

//     import "BDPI" function Bit #(64) bc_c_mem_read (Bit #(64) addr);

uint64_t bc_c_mem_read (uint64_t addr)
{
    uint64_t *p = (uint64_t *) addr;
    uint64_t  x = *p;
#ifdef DEBUG
    printf ("c_mem_read %016llx ==> 0x%016llx\n", addr, x);
#endif
    return x;
}

//     import "BDPI" function Action  bc_c_mem_write (Bit #(64) sz, Bit #(64) a, Bit #(64) x);

void bc_c_mem_write (uint64_t sz, void * addr, uint64_t x)
{
#ifdef DEBUG
    printf ("c_mem_write %p <== 0x%016llx\n", addr, x);
#endif
    switch (sz) {
      case 0: {
	  uint8_t *p8 = addr;
	  *p8 = x;
	  break;
      }
      case 1: {
	  uint16_t *p16 = addr;
	  *p16 = x;
	  break;
      }
      case 2: {
	  uint32_t *p32 = addr;
	  *p32 = x;
	  break;
      }
      case 3: {
	  uint64_t *p64 = addr;
	  *p64 = x;
	  break;
      }
      default: {
	  int isz = sz;
	  fprintf (stderr,
		   "INTERNAL ERROR: c_mem_write: data size = %0d (valid values: 0, 1, 2, 3); ignoring\n",
		   isz);
	  break;
      }
    }
}

// ================================================================
// Convenience defs

typedef union {
    uint64_t  ui64;
    double    d;
    void     *ptr;
} u_t;

uint64_t ptr_to_ui64 (void *p)
{
    u_t u;
    memset (& u, 0, sizeof (u_t));
    u.ptr = p;
    return u.ui64;
}

void * ui64_to_ptr (uint64_t x)
{
    u_t u;
    memset (& u, 0, sizeof (u_t));
    u.ui64 = x;
    return u.ptr;
}

uint64_t double_to_ui64 (double x)
{
    u_t u;
    memset (& u, 0, sizeof (u_t));
    u.d = x;
    return u.ui64;
}

double ui64_to_double (uint64_t x)
{
    u_t u;
    memset (& u, 0, sizeof (u_t));
    u.ui64 = x;
    return u.d;
}

// ================================================================
// The following routine initializes the HW side of the hybrid platform

void  bc_init_HW (char *personality_name)
{
#ifdef FOR_HW
    // ----------------------------------------------------------------
    // Get PDK signature
    if (cny$get_signature_fptr)
	(*cny$get_signature_fptr) (personality_name, &sig, &sig2);
    else {
	fprintf (stderr,"ERROR: cny$get_signature_fptr returned NULL\n");
	exit (1);
    }
    // check that the interleave is binary
    if (cny_cp_interleave() == CNY_MI_3131) {
	printf("ERROR - interleave set to 3131, this personality requires binary interleave\n");
	exit (1);
    }
#endif
}

// ================================================================
// This routine performs the call/return to the HW function.
// The argument is typically a pointer to a parameter block in memory.
// The result is the exception vector from the HW function (normally all zeros).

uint64_t  bc_call_HW (uint64_t  x)
{
    uint64_t ret_exception;

#ifdef FOR_HW
    // Call CAEP function on Convey box, passing 64b arg to BSV in AEG 0
    copcall_fmt(sig, cpCaep, "A", x);

#else
    bc_send_C_to_BSV (x);
    ret_exception = bc_recv_BSV_to_C ();

#endif

    return ret_exception;
}

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

void *bc_malloc (const char   *reason,
		 size_t        size,
		 unsigned int  alignment,
		 Bool          fpga_side,
		 void        **free_this_ptr,
		 Bool          exit_on_failure)
{
    void *p;
    uint64_t ptr;

    if (alignment > 18) {
	fprintf (stderr, "ERROR: bc_malloc alignment is too large for %s\n", reason);
	p = NULL;
	goto done;
    }

    if (! (fpga_side))
        p = malloc (size + (1ULL << alignment));
    else {
        p = cny_cp_malloc (size + (1ULL << alignment));
	// cny_cp_posix_memalign ((void**)&p, 512, size + (1ULL << alignment));
    }

    if (p == NULL) {
	fprintf (stderr, "ERROR: bc_malloc failed for %s\n", reason);
	goto done;
    }

    if (free_this_ptr != NULL)
      *free_this_ptr = p;

    ptr = ptr_to_ui64 (p);
    if ((ptr & ~((1ULL << alignment) - 1)) != ptr) {
      ptr = ((ptr & ~((1ULL << alignment) - 1ULL)) + (1ULL << alignment));
      p = ui64_to_ptr (ptr);
    }

  done:
    if ((p == NULL) && exit_on_failure) exit (1);
    return p;
}

// ================================================================
// The following are user-visible thread management functions (in App_SW)

// Memory barrier/fence:
// If you want two writes W1 and W2 from one thread to be observed in
// that order by another thread, then the first thread should perform
// W1, memory_fence, W2

void bc_memory_barrier (void)
{
#ifdef FOR_HW
    // uses pthread_mutex ops to effect a memory barrier/fence
    pthread_mutex_lock (& mutex_for_mb);
    pthread_mutex_unlock (& mutex_for_mb);
#endif
}

// If the user thread is in an idle loop, it is good to yield the processor

void bc_sched_yield (void)
{
    sched_yield ();
}

// ****************************************************************
// ****************************************************************
// ****************************************************************
// ********* TESTING INFRASTRUCTURE *******************************

// ================================================================
// The following are for standalone testing
// 'main()' represents Bluesim; it creates a pthread for bc_SW_main()
// and becomes the Bluesim thread executing BSV code.
// The two threads synchronize back-and-forth doing 'send' and 'receive'
// (no actual data is transferred, just tests pthread sync code)

#ifdef STANDALONE_TEST

#warning "INFO: Compiling standalone test"

// ----------------

void * bc_SW_main (void *args)
{
    int j;

    for (j = 0; j < 10; j++) {
	printf ("%2d: bc_SW_main: computing\n    ....\n", j);
	sleep (1);

	printf ("%2d: bc_SW_main: computed; waiting to send args\n", j);
	send_wait ("bc_SW_main", & chan_sync_C_to_BSV);
	printf ("%2d: bc_SW_main: sending args\n", j);
	send_done ("bc_SW_main", & chan_sync_C_to_BSV);

	printf ("%2d: bc_SW_main: args sent; waiting for BSV results\n", j);
	recv_wait ("bc_SW_main", & chan_sync_BSV_to_C);
	printf ("%2d: bc_SW_main: receiving results\n", j);
	recv_done ("bc_SW_main", & chan_sync_BSV_to_C);
    }
}

// ----------------

void * BSV_Bluesim_thread (void *args)
{
    int j;

    for (j = 0; j < 10; j++) {
	printf ("    %2d: BSV_Bluesim_thread: waiting for args\n", j);
	recv_wait ("    BSV_Bluesim_thread", & chan_sync_C_to_BSV);
	printf ("    %2d: BSV_Bluesim_thread: receiving args\n", j);
	recv_done ("    BSV_Bluesim_thread", & chan_sync_C_to_BSV);

	printf ("    %2d: BSV_Bluesim_thread: received args; computing\n    ....\n", j);
	sleep (1);

	printf ("    %2d: BSV_Bluesim_thread: computed; waiting to send results\n", j);
	send_wait ("    BSV_Bluesim_thread", & chan_sync_BSV_to_C);
	printf ("    %2d: BSV_Bluesim_thread: sending results\n", j);
	send_done ("    BSV_Bluesim_thread", & chan_sync_BSV_to_C);
    }
}

// ----------------

int main (int argc, char *argv[])
{
    printf ("Initializing\n");

    init_chan_sync ("chan_sync_C_to_BSV", & chan_sync_C_to_BSV);
    init_chan_sync ("chan_sync_BSV_to_C", & chan_sync_BSV_to_C);

    printf ("Creating bc_SW_main\n");
    int status = pthread_create (& bc_SW_main_thread_id, NULL, bc_SW_main, NULL);
    assert_no_error ("main", "pthread_create (bc_SW_main_thread_id)", status);

    printf ("Running  BSV_Bluesim_thread\n");
    BSV_Bluesim_thread (NULL);

    pthread_join (bc_SW_main_thread_id, NULL);

    return 0;
}

#endif

// ================================================================
