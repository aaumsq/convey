#pragma once

// Copyright (c) 2012-2015 Bluespec, Inc., All Rights Reserved
//
// Author: Rishiyur Nikhil, Bluespec, Inc.

// ================================================================
// Thread that monitors the status slots in a parameter block
// and reports changes
// ================================================================

extern void bc_start_instrumentation (uint64_t *status_block_p);

extern void bc_stop_instrumentation (void);
