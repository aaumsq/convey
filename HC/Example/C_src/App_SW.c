// Copyright (c) 2012-2015 Bluespec, Inc., All Rights Reserved
//
// Author: Rishiyur S. Nikhil

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
// The Parameter Block is an array allocated and initialized by the C SW side
// and passed to the BSV HW side.
// The following declares symbolic names for the indexes in the block.

const int NUM_FPGAs = 4;                 // on Convey HC
const int NUM_64b_WORDS_PER_BANK = 8;    // on Convey HC

typedef enum {PARAM_VSIZE,
	      PARAM_VIN1,
	      PARAM_VIN2,
	      PARAM_VOUT,
	      PARAM_PARTIAL_SUM,
	      PARAM_STATUS,
	      PARAM_SENTINEL
} ParamIndexes;

static  uint64_t *param_block, *cp_param_block;
static  int  param_block_size;

// ================================================================
// App_SW()
// This is the real main function
// When running in Bluesim, it is invoked from bc_SW_main()
// When running on Convey HW (or in Convey PDK sim) it is invoked from main()

int App_SW (const char *vsize_s)
{
    uint64_t   vsize = 0;             // Number of 64b words in each vector

    uint64_t  *vin1, *vin2, *vout;             // Host-side input/output vectors
    uint64_t  *cp_vin1, *cp_vin2, *cp_vout;    // CP-side input/output vectors

    uint64_t   actual_sum, expected_sum;

    uint64_t   vsize_per_fpga;
    int        j, fpga, n_errs;

    // ----------------------------------------------------------------
    // Get the vector size from vsize_s (command-line or environment variable)

    if (vsize_s != NULL) vsize = atoi (vsize_s);
    if (vsize == 0) vsize = 100;    // default size is 100
    printf ("C: Vector size is %0lld\n", vsize);

    // ----------------------------------------------------------------
    // Each FPGA will compute the partial sum of a segment of the vectors.
    // Each segment is about 1/4 of the vectors, but we round it up to
    // a 512-Byte boundary (= 64 64-bit words), so that it is aligned with
    // Convey Memory Bank 0.

    vsize_per_fpga = (vsize >> 2);                          // divide by 4
    vsize_per_fpga = ((vsize_per_fpga + 63ull) & (~0x3Full));   // Round up to 64 word boundary

    // ----------------------------------------------------------------
    // Allocate the input and output vectors on HW-side memory

    Bool fpga_side = True;        // Allocate on FPGA side
    Bool exit_on_fail = True;
    unsigned int align512 = 9;    // Align to a 512-Byte boundary (9 lsbs are zero)

    // Malloc on the host
    posix_memalign ((void**)&vin1, 512, vsize * 8);
    posix_memalign ((void**)&vin2, 512, vsize * 8);
    posix_memalign ((void**)&vout, 512, vsize * 8);

    // Malloc on the coproc   
    cny_cp_posix_memalign ((void**)&cp_vin1, 512, vsize * 8);
    cny_cp_posix_memalign ((void**)&cp_vin2, 512, vsize * 8);
    cny_cp_posix_memalign ((void**)&cp_vout, 512, vsize * 8);

    // Initialize the input arrays and compute the expected sum
    expected_sum = 0;
    for (j = 0; j < vsize; j++) {
	vin1 [j] = j;
	vin2 [j] = 2 * j;
	expected_sum += vin1 [j] + vin2 [j];
    }

    // copy the input arrays to coprocessor using datamover
    cny_cp_memcpy (cp_vin1, vin1, vsize*8);
    cny_cp_memcpy (cp_vin2, vin2, vsize*8);

    // ----------------------------------------------------------------
    // Create the param block, and initialize it

    param_block_size = PARAM_SENTINEL * NUM_64b_WORDS_PER_BANK * sizeof (uint64_t);
    posix_memalign ((void**) & param_block, 512, param_block_size);

    cny_cp_posix_memalign ((void**) & cp_param_block, 512, param_block_size);

    printf ("C: param_block addr: 0x%llx, size %0d (64b words)\n",
	    ptr_to_ui64 (param_block), PARAM_SENTINEL * NUM_64b_WORDS_PER_BANK);

    for (fpga = 0; fpga < NUM_FPGAs; fpga++) {
	uint64_t lo = fpga * vsize_per_fpga;
	uint64_t hi = min (vsize, (fpga + 1) * vsize_per_fpga);
	param_block [PARAM_VSIZE * 8 + fpga] = ((lo > hi) ? 0 : (hi - lo));
	param_block [PARAM_VIN1  * 8 + fpga] = ptr_to_ui64 (cp_vin1 + (fpga * vsize_per_fpga));
	param_block [PARAM_VIN2  * 8 + fpga] = ptr_to_ui64 (cp_vin2 + (fpga * vsize_per_fpga));
	param_block [PARAM_VOUT  * 8 + fpga] = ptr_to_ui64 (cp_vout + (fpga * vsize_per_fpga));
	param_block [PARAM_PARTIAL_SUM * 8 + fpga] = 0;

	printf ("C: params [fpga %0d] are %0lld 0x%llx 0x%llx 0x%llx\n", fpga,
		param_block [PARAM_VSIZE * 8 + fpga],
		param_block [PARAM_VIN1  * 8 + fpga],
		param_block [PARAM_VIN2  * 8 + fpga],
		param_block [PARAM_VOUT  * 8 + fpga]);
    }

    // Copy the param block to coprocessor memory
    cny_cp_memcpy (cp_param_block, param_block, param_block_size);

    // ----------------------------------------------------------------
    // Start status monitoring
    // bc_start_instrumentation (& param_block [PARAM_STATUS * 8]);

    // ----------------------------------------------------------------
    // Start the HW computation

    printf ("C: calling HW with address of param block: 0x%0llx\n", ptr_to_ui64 (param_block));
    record_start_time ();
    bc_call_HW (ptr_to_ui64 (param_block));
    record_finish_time ();
    printf ("C: returned from HW\n");
    fprint_delta_time (stdout);

    // ----------------------------------------------------------------
    // Collect the partial sums

    actual_sum = 0;
    for (fpga = 0; fpga < NUM_FPGAs; fpga++) {
	uint64_t partial_sum  = param_block [PARAM_PARTIAL_SUM * 8 + fpga];
	printf ("C: partial sum [%0d] = %0lld\n", fpga, partial_sum);
	actual_sum += partial_sum;
    }
    printf ("C: actual sum = %0lld\n", actual_sum);

    if (expected_sum == actual_sum)
	printf ("C: PASSED (actual = expected)\n");
    else {
	printf ("C: FAILED - expected sum = %lld\n", expected_sum);
	if (expected_sum > actual_sum)
	  printf ("    Actual is less by %lld\n", expected_sum - actual_sum);
	else
	  printf ("    Actual is over by %lld\n", actual_sum - expected_sum);
    }

    // copy the results back from CP to host, with datamover
    cny_cp_memcpy (vout, cp_vout, vsize * 8);

    // Identify which values are wrong, if any
    n_errs = 0;
    for (j = 0; j < vsize; j++) {
      if (vin1 [j] + vin2 [j] != vout [j]) {
	printf ("    Wrong result [%d] = %llu (0x%llx) expecting %llu (0x%llx)\n",
		j, vout [j], vout [j], vin1[j] + vin2[j], vin1[j] + vin2[j]);
	n_errs++;
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
    const char* s = getenv("VECTOR_SIZE");

    if (s == NULL) s = "100";

    App_SW (s);

    return NULL;
}

// ================================================================
// MAIN for execution on Convey Hybrid Computer (not Bluesim)
// The "meat" of the function has been separated into App_SW(),
// which is called at the bottom of this function.

#ifdef FOR_HW

char signature_name [] = "65125";

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
