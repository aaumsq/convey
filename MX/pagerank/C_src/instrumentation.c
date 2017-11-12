// Copyright (c) 2012-2015 Bluespec, Inc., All Rights Reserved
//
// Author: Rishiyur S. Nikhil

// ================================================================
// Thread that monitors the status slots in a parameter block
// and reports changes
// ================================================================

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include <stdint.h>
#include <pthread.h>
#include <time.h>
#include <sched.h>

#include "instrumentation.h"

// ================================================================

#define NUM_FPGAs 4                 // on Convey HC

static uint64_t *status;

static uint64_t last_status [NUM_FPGAs];

static pthread_t  instrumentation_thread_id;

// ================================================================
// This pthread is initiated by the SW app to monitor status

static
void *instrumentation_thread (void * ignored)
{
    int fpga;
    uint64_t s;
    uint32_t r1, r2, w;

    fprintf (stdout, "INSTRUM: start\n");
    while (1) {
	for (fpga = 0; fpga < NUM_FPGAs; fpga++) {
	    s = status [fpga];
	    if (s != last_status [fpga]) {
		r1 = (s >> 40) & 0xFFFFF;
		r2 = (s >> 20) & 0xFFFFF;
	        w =   s        & 0xFFFFF;
		fprintf (stdout, "INSTRUM: FPGA%0d %016llx %0d %0d %0d\n",
			 fpga, s, r1, r2, w);
		last_status [fpga] = s;
	    }
	}
	sched_yield ();
	sleep (1);
    }
}

// ================================================================

void bc_start_instrumentation (uint64_t *status_block_p)
{
    int fpga, stat;

    status = status_block_p;
    
    for (fpga = 0; fpga < NUM_FPGAs; fpga++) {
	status [fpga] = last_status [fpga] = -1;
    }

    stat = pthread_create (& instrumentation_thread_id, NULL, instrumentation_thread, NULL);
    if (stat != 0) {
	fprintf (stderr, "ERROR: could not create instrumentation thread: status = %0d\n", stat);
	exit (1);
    }
}

// ================================================================

void bc_stop_instrumentation (void)
{
    fprintf (stdout, "INSTRUM: stop\n");
    pthread_cancel (instrumentation_thread_id);
}

// ================================================================
