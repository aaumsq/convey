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

#include <iostream>
#include <fstream>
#include <cstdlib>

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

typedef enum {
              PARAM_NODEPTR,
              PARAM_EDGEPTR,
              PARAM_JOBSPTR,
              PARAM_NUMJOBS,
              PARAM_OUTPUT,
              PARAM_STATUS,
              PARAM_SENTINEL
} ParamIndexes;

static  uint64_t *param_block, *cp_param_block;
static  int  param_block_size;

// Later on maybe id and payload should go to 48-bit
struct Node {
    uint32_t id;
    uint32_t payload;
    uint32_t edgePtr;
    uint32_t numEdges;
};

struct Edge {
    uint32_t dest;
    uint32_t weight;
};

struct Job {
    uint32_t priority;
    uint32_t graphId;
};

struct Output {
    uint32_t done;
    uint32_t result;
};

// ASSUMES SRC EDGES ARE IN ORDER
void readGraph(const char* file, Node *nodes, uint32_t &numNodes, Edge* edges, uint32_t &numEdges) {
    
    std::ifstream in(file);
    
    in >> numNodes >> numEdges;
    //in >> numNodes >> ws >> numEdges >> ws;
    
    nodes = (Node*)calloc(numNodes, sizeof(Node));
    edges = (Edge*)calloc(numEdges, sizeof(Edge));
    
    uint32_t src, dest, weight;
    uint32_t lastNode = -1;
    uint32_t edgeIdx = 0;
    while(in.good()) {
        in >> src >> dest >> weight;
        //in >> src >> ws >> dest >> ws >> weight >> ws;
        //cout << "---" << src << " " << dest << " " << weight << endl;
        
        // If new node
        if(src != lastNode) {
            nodes[src].id = src;
            nodes[src].payload = 0;
            nodes[src].edgePtr = edgeIdx;
            nodes[src].numEdges = 0;
        }
        
        // Append new edge
        edges[edgeIdx].dest = dest;
        edges[edgeIdx].weight = weight;
        
        lastNode = src;
        edgeIdx++;
    }
}


// ================================================================
// App_SW()
// This is the real main function
// When running in Bluesim, it is invoked from bc_SW_main()
// When running on Convey HW (or in Convey PDK sim) it is invoked from main()

//int App_SW (const char *vsize_s)
int App_SW (const char *file)
{

    int        j, fpga, n_errs;
    
    
    // Generate and create Graph data structure
    Node *nodes, *cp_nodes;
    Edge *edges, *cp_edges;
    uint32_t numNodes = 0, numEdges = 0;

    readGraph(file, nodes, numNodes, edges, numEdges);
    
    // Generate and create Worklist data structure
    
    Job *jobs, *cp_jobs;
    uint32_t numJobs = 1;
    
    jobs = (Job*)calloc(1, sizeof(Job));
    
    jobs[0].priority = 0;
    jobs[0].graphId = 0;
    
    // Generate and create output
    Output *output, *cp_output;
    
    output = (Output*)calloc(1, sizeof(Output));
    
    // ----------------------------------------------------------------
    // Allocate the input and output vectors on HW-side memory

    // Malloc on the host
    posix_memalign ((void**)&nodes, 512, numNodes*sizeof(Node));
    posix_memalign ((void**)&edges, 512, numEdges*sizeof(Edge));
    posix_memalign ((void**)&cp_jobs, 512, sizeof(Job));
    
    // Malloc on the coproc   
    cny_cp_posix_memalign ((void**)&cp_nodes, 512, numNodes*sizeof(Node));
    cny_cp_posix_memalign ((void**)&cp_edges, 512, numEdges*sizeof(Edge));
    cny_cp_posix_memalign ((void**)&cp_jobs, 512, numJobs*sizeof(Job));
    cny_cp_posix_memalign ((void**)&cp_output, 512, sizeof(Output));
    
    // copy the input arrays to coprocessor using datamover
    cny_cp_memcpy (cp_nodes, nodes, numNodes*sizeof(Node));
    cny_cp_memcpy (cp_edges, edges, numEdges*sizeof(Edge));
    cny_cp_memcpy (cp_jobs, jobs, numJobs*sizeof(Job));
    cny_cp_memcpy (cp_output, output, sizeof(Output));
    
    // ----------------------------------------------------------------
    // Create the param block, and initialize it

    param_block_size = PARAM_SENTINEL * NUM_64b_WORDS_PER_BANK * sizeof (uint64_t);
    posix_memalign ((void**) & param_block, 512, param_block_size);
    cny_cp_posix_memalign ((void**) & cp_param_block, 512, param_block_size);

    printf ("C: param_block addr: 0x%llx, size %0d (64b words)\n",
	    ptr_to_ui64 (param_block), PARAM_SENTINEL * NUM_64b_WORDS_PER_BANK);

    for (fpga = 0; fpga < NUM_FPGAs; fpga++) {
        param_block [PARAM_NODEPTR  * 8 + fpga] = ptr_to_ui64(cp_nodes);
        param_block [PARAM_EDGEPTR  * 8 + fpga] = ptr_to_ui64(cp_edges); 
        param_block [PARAM_JOBSPTR  * 8 + fpga] = ptr_to_ui64(cp_jobs);
        param_block [PARAM_NUMJOBS  * 8 + fpga] = numJobs;
        param_block [PARAM_OUTPUT   * 8 + fpga] = ptr_to_ui64(cp_output);
        param_block [PARAM_SENTINEL * 8 + fpga] = 0xCAFEF00D;
        
        printf ("C: params [fpga %0d] are %0llx 0x%llx 0x%llx 0x%lld 0x%llx 0x%lld\n", fpga,
                param_block [PARAM_NODEPTR  * 8 + fpga],
                param_block [PARAM_EDGEPTR   * 8 + fpga],
                param_block [PARAM_JOBSPTR   * 8 + fpga],
                param_block [PARAM_NUMJOBS   * 8 + fpga],
                param_block [PARAM_OUTPUT    * 8 + fpga],
                param_block [PARAM_SENTINEL  * 8 + fpga]);
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
    cny_cp_memcpy (param_block, cp_param_block, param_block_size);

    for (fpga = 0; fpga < NUM_FPGAs; fpga++) {
        Output* out = (Output*)(&(param_block [PARAM_OUTPUT * 8 + fpga]));
        printf ("C: partial sum [%0d] = %0llx, %0lld\n", fpga, out->done, out->result);
    }

    // ----------------------------------------------------------------
    // Stop status monitoring
    // bc_stop_instrumentation ();

    // ----------------------------------------------------------------
    return 0;
}

// ================================================================
// MAIN for Bluesim (this is pthread_create'd by the BSV testbench)
extern "C" {
void *bc_SW_main (void * ignored)
{
    const char* s = getenv("INPUT_FILE");

    if (s == NULL) s = "./test.edgelist";

    App_SW (s);

    return NULL;
}
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
        const char* s = getenv("INPUT_FILE");
        return App_SW (s);
    }
}

#endif
