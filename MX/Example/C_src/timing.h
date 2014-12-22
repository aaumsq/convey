#pragma once

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

extern void record_start_time (void);

extern void record_finish_time (void);

extern void delta_time (long long *full_ns, long *s, long *ms, long *us, long *ns);

extern void fprint_delta_time (FILE *fp);
