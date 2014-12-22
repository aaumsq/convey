// Copyright (c) 2013-2015 Bluespec, Inc., All Rights Reserved
//
// Author: Rishiyur Nikhil, Bluespec, Inc.

// ================================================================
// This is the SW side of an app to run on Convey Hybrid Computers

// ================================================================

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include <stdint.h>
#include <pthread.h>
#include <time.h>

#include "BC_linkage.h"
#include "timing.h"
#include "instrumentation.h"

#define max(a,b) (((a) > (b)) ? (a) : (b))
#define min(a,b) (((a) < (b)) ? (a) : (b))

// ================================================================
// Matrix and block dimensions, for testing

uint64_t mb = 2, mm = 1;
uint64_t nb = 3, nn = 1;
uint64_t pb = 4, pp = 1;

int aix (int i, int k) { return (i * nb * nn) + k; }
int bix (int k, int j) { return (k * pb * pp) + j; }
int cix (int i, int j) { return (i * pb * pp) + j; }

// ================================================================

const int NUM_FPGAS = 4;          // on Convey HC

const int ARGC_MAX  = 256;

uint64_t *argv;

const int64_t m = 2, n = 3, p = 4;

int64_t *A, *B, *C0, *C1, *C2, *C3, *Cexp;

// ================================================================
// App_SW()
// This is the real main function
// When running in Bluesim, it is invoked from bc_SW_main()
// When running on Convey HW (or in Convey PDK sim) it is invoked from main()

int App_SW ()
{
    int i, k, j, fpga, n_errs;
    int64_t x;

    // ----------------------------------------------------------------
    // Allocate the input and output vectors on HW-side memory

    Bool fpga_side = True;        // Allocate on FPGA side
    Bool exit_on_fail = True;
    unsigned int align512 = 9;    // Align to a 512-Byte boundary (9 lsbs are zero)

    A    = (int64_t *) bc_malloc ("A",  m * n * 8, align512, fpga_side, NULL, exit_on_fail);
    B    = (int64_t *) bc_malloc ("B",  n * p * 8, align512, fpga_side, NULL, exit_on_fail);
    C0   = (int64_t *) bc_malloc ("C1", m * p * 8, align512, fpga_side, NULL, exit_on_fail);
    C1   = (int64_t *) bc_malloc ("C1", m * p * 8, align512, fpga_side, NULL, exit_on_fail);
    C2   = (int64_t *) bc_malloc ("C1", m * p * 8, align512, fpga_side, NULL, exit_on_fail);
    C3   = (int64_t *) bc_malloc ("C1", m * p * 8, align512, fpga_side, NULL, exit_on_fail);
    Cexp = (int64_t *) bc_malloc ("C1", m * p * 8, align512, fpga_side, NULL, exit_on_fail);

    // Initialize the input arrays and compute the expected outputs
    x = 2;
    for (i = 0; i < m; i++)
	for (k = 0; k < n; k++)
	    A[aix(i,k)] = x++;

    x = 2;
    for (k = 0; k < n; k++)
	for (j = 0; j < p; j++)
	    B[bix(k,j)] = x++;

    printf ("Expected output\n");
    for (i = 0; i < m; i++) {
	printf ("  %2d:", i);
	for (j = 0; j < p; j++) {
	    C0[cix(i,j)]   = 0;
	    C1[cix(i,j)]   = 0;
	    C2[cix(i,j)]   = 0;
	    C3[cix(i,j)]   = 0;
	    Cexp[cix(i,j)] = 0;
	    for (k = 0; k < n; k++)
		Cexp[cix(i,j)] += A[aix(i,k)] * B[bix(k,j)];
	    printf (" %8x", Cexp[cix(i,j)]);
	}
	printf ("\n");
    }

    // ----------------------------------------------------------------
    // Create the param block, and initialize it

    argv = (uint64_t *) bc_malloc ("param_block",
				   (NUM_FPGAS * ARGC_MAX * sizeof (uint64_t)),
				   align512, (! fpga_side), NULL, exit_on_fail);

    printf ("C: argv addr: 0x%llx, size %0d (64b words)\n",
	    ptr_to_ui64 (argv), NUM_FPGAS * ARGC_MAX);

    // Set up argv for FPGA 0
    argv [0] = 22;
    argv [1] = ptr_to_ui64 (A);
    argv [2] = ptr_to_ui64 (B);
    argv [3] = ptr_to_ui64 (C0);
    argv [4] = mb; argv [5] = nb; argv [6] = pb;
    argv [7] = mm; argv [8] = nn; argv [9] = pp;

    argv [10] = nb * nn * 8;         // dAi1
    argv [11] = 1 * 8;               // dAk1
    argv [12] = nb * nn * mm * 8;    // dAib
    argv [13] = nn * 8;              // dAkb

    argv [14] = pb * pp * 8;         // dBk1
    argv [15] = 1 * 8;               // dBj1
    argv [16] = pb * pp * nn * 8;    // dBkb
    argv [17] = pp * 8;              // dBjb

    argv [18] = pb * pp * 8;         // dCi1
    argv [19] = 1 * 8;               // dCj1
    argv [20] = pb * pp * mm * 8;    // dCib
    argv [21] = pp * 8;              // dCjb

    printf ("    A: %p    B: %p\n", A, B);
    printf ("    C0: %p\n", C0);

    // Set up argv for FPGAs 1, 2, 3
    memcpy (& argv [1 * ARGC_MAX], argv, ARGC_MAX * sizeof (uint64_t));
    argv [1 * ARGC_MAX + 3] = ptr_to_ui64 (C1);
    printf ("    C1: %p\n", C1);

    memcpy (& argv [2 * ARGC_MAX], argv, ARGC_MAX * sizeof (uint64_t));
    argv [2 * ARGC_MAX + 3] = ptr_to_ui64 (C2);
    printf ("    C2: %p\n", C2);

    memcpy (& argv [3 * ARGC_MAX], argv, ARGC_MAX * sizeof (uint64_t));
    argv [3 * ARGC_MAX + 3] = ptr_to_ui64 (C3);
    printf ("    C3: %p\n", C3);

    // Make sure writes have reached memory
    bc_memory_barrier ();

    // ----------------------------------------------------------------
    // Start status monitoring
    // bc_start_instrumentation (& param_block [PARAM_STATUS * 8]);

    // ----------------------------------------------------------------
    // Start the HW computation

    printf ("C: calling HW with address of param block: 0x%0llx\n", ptr_to_ui64 (argv));
    record_start_time ();
    bc_call_HW (ptr_to_ui64 (argv));
    record_finish_time ();
    printf ("C: returned from HW\n");
    fprint_delta_time (stdout);

    // ----------------------------------------------------------------
    // Compare actual with expected results

    n_errs = 0;
    for (i = 0; i < (mb * mm); i++) {
	for (j = 0; j < (pb * pp); j++) {
	    if (C0[cix(i,j)] != Cexp[cix(i,j)]) n_errs++;
	    if (C1[cix(i,j)] != Cexp[cix(i,j)]) n_errs++;
	    if (C2[cix(i,j)] != Cexp[cix(i,j)]) n_errs++;
	    if (C3[cix(i,j)] != Cexp[cix(i,j)]) n_errs++;
	}
    }
    printf ("    Number of locations in error: %d\n", n_errs);

    // ----------------------------------------------------------------
    // Stop status monitoring
    // bc_stop_instrumentation ();

    // ----------------------------------------------------------------
    return 0;
}

// ================================================================
// MAIN for Bluesim (this is pthread_create'd by the BSV testbench)

void *bc_SW_main (void * ignored)
{
    App_SW ();

    return NULL;
}

// ================================================================
// MAIN for execution on Convey Hybrid Computer (not Bluesim)
// The "meat" of the function has been separated into App_SW(),
// which is called at the bottom of this function.

#ifdef FOR_HW

char signature_name [] = "65123";

int main (int argc, char *argv[])
{
    if (argc > 2) {
	fprintf (stderr, "Usage:    %s  [<vector size>]\n", argv [0]);
	fprintf (stderr, "    or, set the VECTOR_SIZE environment variable\n");
	fprintf (stderr, "    or, the program uses the default size: 100\n");
	exit (1);
    }

    // ----------------
    // Do initializations of convey HW
    printf ("Initializing HW for signature %s\n", signature_name);
    bc_init_HW (signature_name);

    // ----------------
    if (argc == 2)
        return App_SW (argv [1]);
    else {
        const char* s = getenv("VECTOR_SIZE");
	return App_SW (s);
    }
}

#endif
