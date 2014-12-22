// Copyright (c) 2012-2015 Bluespec, Inc., All Rights Reserved
//
// Author: Rishiyur S. Nikhil

// ================================================================
// Measuring real elapsed time
//
// Call record_start_time() and record_finish_time() before and
// after your computation, then call delta_time() for the diff
// Or call fprint_delta_time to print it
// ================================================================

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "timing.h"

// ================================================================
// FOR APPLE MAC OSX ONLY
// clock_gettime is not defined on Mac OSX.
// Here, we just define it as a no-op, since we don't measure on a Mac anyway

#ifdef __APPLE__
#define CLOCK_MONOTONIC 0
int clock_gettime (int clk_id, struct timespec *tp)
{
    return 0;
}
#endif

// ================================================================

static    struct timespec start_time;
static    struct timespec finish_time;
static    long long  start_time_ns;
static    long long  finish_time_ns;

void record_start_time (void)
{
    if (clock_gettime(CLOCK_MONOTONIC, &start_time) != 0) {
	perror("start time");
	exit(1);
    }
    start_time_ns = start_time.tv_sec * 1000000000 + start_time.tv_nsec;
    // printf("Start time: %0lld ns\n", start_time_ns);
}

void record_finish_time (void)
{
    if (clock_gettime(CLOCK_MONOTONIC, &finish_time) != 0) {
	perror("finish time");
	exit(1);
    }     
    finish_time_ns = finish_time.tv_sec * 1000000000 + finish_time.tv_nsec;
    // printf("Finish time: %0lld ns\n", finish_time_ns);
}

void delta_time (long long *full_ns, long *s, long *ms, long *us, long *ns)
{
    long long delta_ns = finish_time_ns - start_time_ns;
    if (full_ns != NULL) *full_ns = delta_ns;
    *ns      = delta_ns % 1000; delta_ns = delta_ns / 1000;
    *us      = delta_ns % 1000; delta_ns = delta_ns / 1000;
    *ms      = delta_ns % 1000; delta_ns = delta_ns / 1000;
    *s       = delta_ns % 1000; delta_ns = delta_ns / 1000;
}

void fprint_delta_time (FILE *fp)
{
    long long full_ns;
    long s, ms, us, ns;

    delta_time (& full_ns, & s, & ms, & us, & ns);

    fprintf(fp, "Delta time = %0ld.%03ld,%03ld,%03ld secs\n", s, ms, us, ns);
}

// ================================================================
// A small testbench

/*
#include <unistd.h>

int main (int argc, char *argv[])
{
    if (argc < 2) {
	fprintf (stderr, "Usage:  %s  <sleep_seconds>", argv[0]);
	exit (1);
    }

    record_start_time();

    sleep (atoi (argv[1]));

    record_finish_time ();
    fprint_delta_time (stdout);
}
*/
