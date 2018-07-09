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

#ifdef FOR_HW
#include <convey/usr/cny_comp.h>
#endif

#include <iostream>
#include <fstream>
#include <cstdlib>

#include <assert.h>

//#include "BC_linkage.h"
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
              PARAM_META,
              PARAM_OUTPUT,
              PARAM_DONE,
              PARAM_SENTINEL
} ParamIndexes;

typedef enum {
    LOCK_W,
    LOCK_R,
    OFFSET,
    HEADPTR0,
    HEADPTR1,
    HEADPTR2,
    HEADPTR3,
    HEADPTR4,
    HEADPTR5,
    HEADPTR6,
    HEADPTR7,
    TAILPTR0,
    TAILPTR1,
    TAILPTR2,
    TAILPTR3,
    TAILPTR4,
    TAILPTR5,
    TAILPTR6,
    TAILPTR7,
    WLSIZE,
    BSIZE,
    NUMFPGA,
    TAILPTR_W0,
    TAILPTR_W1,
    TAILPTR_W2,
    TAILPTR_W3,
    TAILPTR_W4,
    TAILPTR_W5,
    TAILPTR_W6,
    TAILPTR_W7,
    COMMITHEAD,
    COMMITTAIL
} JobMetaIndexes;

static  uint64_t *param_block, *cp_param_block;
static  int  param_block_size;

// Later on maybe id and payload should go to 48-bit
/* FPGA
struct Node {
    uint32_t id;
    uint32_t payload;
    uint32_t edgePtr;
    uint32_t numEdges;
};*/

struct Node {
    uint32_t edgePtr;
    uint32_t id;
    uint32_t numEdges;
    uint32_t payload;
    uint32_t edgePtr2;
    uint32_t id2;
    uint32_t numEdges2;
    uint32_t payload2;
};

/* FPGA
struct Edge {
    uint32_t dest;
    uint32_t weight;
};
*/

struct Edge {
    uint32_t weight;
    uint32_t dest;
};

/* FPGA
struct Job {
    uint32_t priority;
    uint32_t graphId;
};
*/

struct Job {
    uint32_t graphId;
    uint32_t priority;
};

struct Output {
    uint32_t done;
    uint32_t result;
};

struct Done {
    uint32_t done0;
    uint32_t done1;
    uint32_t done2;
    uint32_t done3;
};

// ASSUMES SRC EDGES ARE IN ORDER
void readGraph(const char* file, Node **nodes, uint32_t &numNodes, Edge **edges, uint32_t &numEdges) {
    
    std::ifstream in(file);
    
    in >> numNodes >> numEdges;
    std::cout << "numNodes: " << numNodes << ", numEdges: " << numEdges << "\n";
    posix_memalign((void**)nodes, 512, numNodes*sizeof(Node));
    posix_memalign((void**)edges, 512, numEdges*sizeof(Edge));
    printf("Sizeof node: %d, sizeof edge: %d\n", sizeof(Node), sizeof(Edge));
    std::cout << "nodes: " << numNodes*sizeof(Node) << "B, edges: " << numEdges*sizeof(Edge) << "B\n";
    for (int i = 0; i < numNodes; i++) {
        (*nodes)[i].id = i;
        (*nodes)[i].payload = -1;
        (*nodes)[i].numEdges = 0;
        (*nodes)[i].edgePtr = 0;
        (*nodes)[i].id2 = i;
        (*nodes)[i].payload2 = -1;
        (*nodes)[i].numEdges2 = 0;
        (*nodes)[i].edgePtr2 = 0;
    }
   
    uint32_t src, dest, weight;
    uint32_t lastNode = -1;
    uint32_t edgeIdx = 0;
    while(in.good() && edgeIdx < numEdges) {
        in >> src >> dest >> weight;
        //std::cout << "---" << src << " " << dest << " " << weight << ", idx: " << edgeIdx << "\n";
        
        // If new node
        if(src != lastNode) {
            assert(src < numNodes);
            (*nodes)[src].id = src;
            (*nodes)[src].payload = -1;
            (*nodes)[src].edgePtr = edgeIdx;
            (*nodes)[src].numEdges = 0;
            (*nodes)[src].id2 = src;
            (*nodes)[src].payload2 = -1;
            (*nodes)[src].edgePtr2 = edgeIdx;
            (*nodes)[src].numEdges2 = 0;
        }
        
        // Append new edge
        assert(edgeIdx < numEdges);
        if(edgeIdx >= numEdges)
            printf("ERROR: node %d edgeIdx %d is not less than numEdges %d (src %d, dest %d, weight %d)\n", src, edgeIdx, numEdges, src, dest, weight); 
        (*edges)[edgeIdx].dest = dest;
        (*edges)[edgeIdx].weight = weight;
        (*nodes)[src].numEdges += 1;
        (*nodes)[src].numEdges2 += 1;
        
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
int App_SW (const char *bsize, const char *file)
{

    int        j, fpga, n_errs;
    
    
    std::ofstream out("output");
    
    // Generate and create Graph data structure
    Node *nodes, *cp_nodes;
    Edge *edges, *cp_edges;
    uint32_t numNodes = 0, numEdges = 0;

    readGraph(file, &nodes, numNodes, &edges, numEdges);
    printf("Done reading graph\n");
    printf("numNodes: %d, numEdges: %d\n", numNodes, numEdges);
    /*for(int i = 0; i < numNodes; i++)
        printf("  nodes[%d] = {%d, %d, %d, %d}\n", i, nodes[i].id, nodes[i].payload, nodes[i].edgePtr, nodes[i].numEdges);
    
    for(int i = 0; i < numEdges; i++)
        printf("  edges[%d] = {%d, %d}]\n", i, edges[i].dest, edges[i].weight);
    */
    // Generate and create Worklist data structure
    
    Job *jobs, *cp_jobs;
    uint32_t numJobs = 1;
    uint64_t maxJobs = (uint64_t)1024*1024*1024; // 2 Giga-entries
    posix_memalign((void**)&jobs, 512, numJobs*sizeof(Job));
    
    #define INIT_IDX 0
    #define INIT_IDX2 1
    
    jobs[0].priority = 0;
    jobs[0].graphId = INIT_IDX;
    nodes[INIT_IDX].payload = 0;
    jobs[1].priority = 0x80000000;
    jobs[1].graphId = INIT_IDX2;
    nodes[INIT_IDX2].payload2 = 0;
    
    uint64_t *meta, *cp_meta;
    uint32_t numMeta = 32;
    uint64_t bias = 0;
    uint64_t bucket_size = atoi(bsize);
    posix_memalign((void**)&meta, 512, numMeta*sizeof(uint64_t));
    
    meta[LOCK_W] = 0;
    meta[LOCK_R] = 0;
    meta[OFFSET] = 0;
    meta[HEADPTR0] = 0;
    meta[HEADPTR1] = 0;
    meta[HEADPTR2] = 0;
    meta[HEADPTR3] = 0;
    meta[HEADPTR4] = 0;
    meta[HEADPTR5] = 0;
    meta[HEADPTR6] = 0;
    meta[HEADPTR7] = 0;
    meta[TAILPTR0] = numJobs;
    meta[TAILPTR1] = 0;
    meta[TAILPTR2] = 0;
    meta[TAILPTR3] = 0;
    meta[TAILPTR4] = 0;
    meta[TAILPTR5] = 0;
    meta[TAILPTR6] = 0;
    meta[TAILPTR7] = 0;
    meta[WLSIZE] = maxJobs;
    meta[BSIZE] = ((bias << 32) | bucket_size);
    meta[NUMFPGA] = 3;
    meta[TAILPTR_W0] = numJobs;
    meta[TAILPTR_W1] = 0;
    meta[TAILPTR_W2] = 0;
    meta[TAILPTR_W3] = 0;
    meta[TAILPTR_W4] = 0;
    meta[TAILPTR_W5] = 0;
    meta[TAILPTR_W6] = 0;
    meta[TAILPTR_W7] = 0;
    meta[COMMITHEAD] = 1;
    meta[COMMITTAIL] = 0;

    printf("meta[BSIZE] is %0x\n", ((bias << 32) | bucket_size));
    
    // Generate and create output
    //Output *output, *cp_output;
    
    //output = (Output*)calloc(1, sizeof(Output));
    uint64_t *output, *cp_output;

    output = (uint64_t*)calloc(32, sizeof(uint64_t));
    for (int i = 0; i < 32; i++)
        output[i] = 0;
    
    // Done signals
    Done *done, *cp_done;
    
    done = (Done*)calloc(1, sizeof(Done));
    
    // ----------------------------------------------------------------
    // Allocate the input and output vectors on HW-side memory
    printf("Allocating CNY memory\n");
    //for(int i = 0; i < numNodes*sizeof(Node)/4; i++)
    //    printf("%d\n", ((int*)nodes)[i]);
    // Malloc on the coproc   
    cny_cp_posix_memalign((void**)&cp_nodes, 512, numNodes*sizeof(Node));
    cny_cp_posix_memalign((void**)&cp_edges, 512, numEdges*sizeof(Edge));
    cny_cp_posix_memalign((void**)&cp_jobs, 512, maxJobs*sizeof(Job));
    cny_cp_posix_memalign((void**)&cp_meta, 512, numMeta*sizeof(uint64_t));
    cny_cp_posix_memalign((void**)&cp_output, 512, 32*sizeof(uint64_t));
    cny_cp_posix_memalign((void**)&cp_done, 512, 1*sizeof(Done));
    
    // copy the input arrays to coprocessor using datamover
    cny_cp_memcpy (cp_nodes, nodes, numNodes*sizeof(Node));
    cny_cp_memcpy (cp_edges, edges, numEdges*sizeof(Edge));
    cny_cp_memcpy (cp_jobs, jobs, numJobs*sizeof(Job));
    cny_cp_memcpy (cp_meta, meta, numMeta*sizeof(uint64_t));
    cny_cp_memcpy (cp_output, output, 32*sizeof(uint64_t));
    cny_cp_memcpy (cp_done, done, 1*sizeof(Done));
    
    // ----------------------------------------------------------------
    // Create the param block, and initialize it
    printf("Creating param block\n");
    param_block_size = (PARAM_SENTINEL+1) * NUM_64b_WORDS_PER_BANK * sizeof (uint64_t);
    posix_memalign ((void**) & param_block, 512, param_block_size);
    cny_cp_posix_memalign ((void**) & cp_param_block, 512, param_block_size);

    printf ("C: param_block addr: 0x%llx, size %0d (64b words)\n",
            ptr_to_ui64 (param_block), (PARAM_SENTINEL+1) * NUM_64b_WORDS_PER_BANK);

    for (fpga = 0; fpga < NUM_FPGAs; fpga++) {
        param_block [PARAM_NODEPTR  * 8 + fpga] = ptr_to_ui64(cp_nodes);
        param_block [PARAM_EDGEPTR  * 8 + fpga] = ptr_to_ui64(cp_edges); 
        param_block [PARAM_JOBSPTR  * 8 + fpga] = ptr_to_ui64(cp_jobs);
        param_block [PARAM_META     * 8 + fpga] = ptr_to_ui64(cp_meta);
        param_block [PARAM_OUTPUT   * 8 + fpga] = ptr_to_ui64(cp_output);
        //param_block [PARAM_OUTPUT   * 8 + fpga] = 0;
        param_block [PARAM_DONE     * 8 + fpga] = ptr_to_ui64(cp_done);
        param_block [PARAM_SENTINEL * 8 + fpga] = 0xCAFEF000+fpga;
        printf("%0d\n", PARAM_SENTINEL * 8 + fpga);
        printf ("C: params [fpga %0d] are %0llx 0x%llx 0x%llx %lld 0x%llx 0x%llx\n", fpga,
                param_block [PARAM_NODEPTR  * 8 + fpga],
                param_block [PARAM_EDGEPTR  * 8 + fpga],
                param_block [PARAM_JOBSPTR  * 8 + fpga],
                param_block [PARAM_META     * 8 + fpga],
                param_block [PARAM_OUTPUT   * 8 + fpga],
                param_block [PARAM_DONE     * 8 + fpga],
                param_block [PARAM_SENTINEL * 8 + fpga]);
    }

    // Copy the param block to coprocessor memory
    cny_cp_memcpy (cp_param_block, param_block, param_block_size);

    // ----------------------------------------------------------------
    // Start status monitoring
    // bc_start_instrumentation (& param_block [PARAM_STATUS * 8]);

    // ----------------------------------------------------------------
    // Start the HW computation

    printf ("C: calling HW with address of param block: 0x%0llx\n", ptr_to_ui64 (cp_param_block));
    record_start_time ();
    bc_call_HW (ptr_to_ui64 (param_block));
    record_finish_time ();
    printf ("C: returned from HW\n");
    fprint_delta_time (stdout);
    
    cny_cp_memcpy(output, cp_output, 32*sizeof(uint64_t));
    uint64_t total_nodes = 0, total_edges = 0, total_retry = 0, total_egstall = 0, total_mem = 0, total_wlstall = 0;
    for (fpga = 0; fpga < 4; fpga++) {
	//uint64_t partial_sum  = param_block [PARAM_OUTPUT * 8 + fpga];
	uint64_t partial_nodes = output[fpga*8];
	uint64_t partial_edges = output[fpga*8+1];
	uint64_t partial_retry = output[fpga*8+2];
	uint64_t partial_egstall = output[fpga*8+3];
	uint64_t partial_wlstall = output[fpga*8+4];
	uint64_t partial_mem = output[fpga*8+5];
	total_nodes += partial_nodes;
	total_edges += partial_edges;
	total_retry += partial_retry;
	total_egstall += partial_egstall;
	total_wlstall += partial_wlstall;
	total_mem += partial_mem;
	printf ("C: partial node fetched [%0d] = %0lld\n", fpga, partial_nodes);
	printf ("C: partial edge fetched [%0d] = %0lld\n", fpga, partial_edges);
        printf ("C: CASRetried[%0d]: %0lld\n", fpga, partial_retry);
        //for (int i = 0; i < 16; i++) {
        //    printf("C: memWorklist[%0d][%0d]: %0lld\n", fpga, i, output[fpga*64+i+2]);
        //}
        //for (int i = 16; i < 32; i++) {
        //    printf("C: memGraph[%0d][%0d]: %0lld\n", fpga, i-16, output[fpga*64+i+2]);
        //}
        //for (int i = 32; i < 48; i++) {
        //    printf("C: memSSSP[%0d][%0d]: %0lld\n", fpga, i-32, output[fpga*64+i+2]);
        //}
    }
    printf("C: Nodes fetched: %0lld\n", total_nodes);
    printf("C: Edges fetched: %0lld\n", total_edges);
    printf("C: Total CASRetried: %0lld\n", total_retry);
    printf("C: Total stall by edgePipe cycles: %0lld\n", total_egstall);
    printf("C: Total stall by worklist cycles: %0lld\n", total_wlstall);
    printf("C: Total memory accesses: %0lld\n", total_mem);
    uint64_t cycles = param_block[PARAM_SENTINEL * 8];
    printf("C: Total cycles: %0lld\n", cycles);
    // Check all done
    //cny_cp_memcpy(done, cp_done, 1*sizeof(Done));
    //assert((dones->done0 == 2) && (dones->done1 == 2) && (dones->done2 == 2) && (dones->done3 == 2));
    //printf("Dones: %d %d %d %d", done->done0, done->done1, done->done2, done->done3);
    
    // ----------------------------------------------------------------
    // Print out new node payloads
    cny_cp_memcpy (nodes, cp_nodes, numNodes*sizeof(Node));
    out << "start node: " << INIT_IDX << ", " << INIT_IDX2 << "\n";
    for(int i = 0; i < numNodes; i++) {
        //printf("  nodes[%d] = {%d, *%d, %d, %d}\n", i, nodes[i].id, nodes[i].payload, nodes[i].edgePtr, nodes[i].numEdges);
        out << nodes[i].id << "," << nodes[i].payload << ", " << nodes[i].payload2 << "\n";
    }
    out.close();
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
    const char* b = getenv("BSIZE");

    //if (s == NULL) s = "./test_bc.edgelist";
    if (s == NULL) {
        s = "./test.edgelist";
        b = "6";
    } 

    App_SW (b, s);

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
    if (argc != 3) {
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
    if (argc == 3)
        return App_SW (argv [1], argv [2]);
    else {
        const char* s = getenv("INPUT_FILE");
        return App_SW ("6", s);
    }
}

#endif
