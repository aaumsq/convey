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
#include <vector>

#include <assert.h>

//#include "BC_linkage.h"
#include "BC_linkage.h"
#include "timing.h"
#include "instrumentation.h"

#ifdef FOR_HW
#include <convey/usr/cny_comp.h>
#endif

#define max(a,b) (((a) > (b)) ? (a) : (b))
#define min(a,b) (((a) < (b)) ? (a) : (b))

#define CHK_FREAD(ptr, sz, count, stream) \
    if (fread(ptr, sz, count, stream) != count) \
    { \
    std::cerr << "error reading lonestar binary CSR file " << std::endl; \
    exit(1); \
    } \

// ================================================================
// The Parameter Block is an array allocated and initialized by the C SW side
// and passed to the BSV HW side.
// The following declares symbolic names for the indexes in the block.

const int NUM_FPGAs = 4;                 // on Convey HC
const int NUM_64b_WORDS_PER_BANK = 8;    // on Convey HC

float teleportation;
float tolerance;

struct BigNode {
    uint64_t edgePtr;
    uint64_t numEdges;
    uint64_t incomingEdgePtr;
    uint64_t numIncomingEdges;
    uint64_t payload;
    int64_t  excessFlow;
    double   pagerank;
    uint64_t current;
};
    
struct BigEdge {
    uint64_t weight;
    uint64_t dest;
    BigEdge(uint64_t w, uint64_t d) {
    weight = w;
    dest = d;
    }
};


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
    HEADPTR,
    TAILPTR,
    WLSIZE,
    NUMFPGA,
    TAILPTR_W,
    COMMITHEAD,
    COMMITTAIL,
    TOLERANCE
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
    uint32_t numEdges;
    uint32_t payload;
    uint32_t id;
    uint32_t residual;
    uint32_t numEdges_inv;
    uint32_t reserved1;
    uint32_t reserved2;
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
void readGraph(const char* file, Node **nodes, uint64_t &numNodes, Edge **edges, uint64_t &numEdges) {
    printf("Opening gr32 format file\n");
    
    FILE* f = fopen(file, "r");
    if(!f) {
      std::cerr << "Unable to open file " << file << "\n";
      exit(1);
    }
    
    uint64_t version;
    CHK_FREAD(&version, 8, 1, f);
    printf("File version = %lu\n", version);
    
    uint64_t sizeEdgeType;
    CHK_FREAD(&sizeEdgeType, 8, 1, f);
    printf("File edge data size = %lu bytes\n", sizeEdgeType);
    
    CHK_FREAD(&numNodes, 8, 1, f);
    CHK_FREAD(&numEdges, 8, 1, f);
    printf("File numNodes = %lu, numEdges = %lu\n", numNodes, numEdges);
    
    int success = posix_memalign((void**)nodes, 512, numNodes*sizeof(Node));
    success = posix_memalign((void**)edges, 512, numEdges*sizeof(Edge));

    //std::ifstream in(file);
    float *residuals;
    
    //in >> numNodes >> numEdges;
    //std::cout << "numNodes: " << numNodes << ", numEdges: " << numEdges << "\n";
    //posix_memalign((void**)nodes, 512, numNodes*sizeof(Node));
    //posix_memalign((void**)edges, 512, numEdges*sizeof(Edge));
    residuals = new float [numNodes];
    //pageranks = new float [numNodes];

    printf("Sizeof node: %d, sizeof edge: %d\n", sizeof(Node), sizeof(Edge));
    //std::cout << "nodes: " << numNodes*sizeof(Node) << "B, edges: " << numEdges*sizeof(Edge) << "B\n";
    
    // Read node edgePtrs
    //std::vector<uint32_t> tmp(numNodes);
    uint32_t* tmp = (uint32_t*) malloc(sizeof(uint32_t)*numNodes);
    CHK_FREAD(tmp, 4, numNodes, f);

    float initPagerank = 1.0 - teleportation;
    int initPagerank_int = *(int*)&initPagerank;
    for (int i = 0; i < numNodes; i++) {
        //printf("node %0d, edgePtr %0d\n", i, tmp[i]);
        (*nodes)[i].id = i;
        (*nodes)[i].payload = initPagerank_int;
        (*nodes)[i].edgePtr = tmp[i];
        if(i != numNodes-1)
          assert(tmp[i+1] >= tmp[i]);

	if(i == (numNodes - 1))
	  (*nodes)[i].numEdges = numEdges - tmp[i];
	else
	  (*nodes)[i].numEdges = tmp[i+1] - tmp[i];

	(*nodes)[i].residual = 0;
        residuals[i] = 0.0;
	//pageranks[i] = initPagerank;
    }
   
    // Read in edge destinations
    //tmp = std::vector<uint32_t>(numEdges);
    uint32_t* tmp2 = (uint32_t*) malloc(sizeof(uint32_t)*numEdges);
    CHK_FREAD(tmp2, 4, numEdges, f);

    for(uint64_t i = 0; i < numEdges; i++) {
      (*edges)[i].dest = tmp2[i];
      (*edges)[i].weight = 0;
    }

    // Read edge weights (if they exist)
    if(sizeEdgeType != 0) {
      assert(sizeEdgeType == 4);
      CHK_FREAD(tmp2, sizeEdgeType, numEdges, f);

      for(uint64_t i = 0; i < numEdges; i++) {
        (*edges)[i].weight = tmp2[i];
      }
    }

    for (int i = 0; i < numNodes; i++) {
        int cur_numEdges = (*nodes)[i].numEdges;
        for (int j = 0; j < cur_numEdges; j++) {
	    int k = (*nodes)[i].edgePtr;
	    int dest = (*edges)[k+j].dest;
	    residuals[dest] += (1.0/float(cur_numEdges));
	}
    }

    //for (int i = 0; i < numNodes; i++) {
    //    int cur_numEdges = (*nodes)[i].numEdges;
    //    float oldResidual = residuals[i];
    //    float oldPagerank = pageranks[i];
    //    float addResidual = oldResidual * teleportation / float(cur_numEdges);
    //    pageranks[i] += residuals[i];
    //    residuals[i] = 0.0;

    //    for (int j = 0; j < cur_numEdges; j++) {
    //        int k = (*nodes)[i].edgePtr;
    //        int dest = (*edges)[k+j].dest;
    //        float oldDestResidual = residual[dest];
    //        residual[dest] += addResidual;
    //        if (oldDestResidual < tolerance && residual[dest] >= tolerance)
    //            worklist.push_back(dest);
    //    }
    //}
	    

    for (int i = 0; i < numNodes; i++) {
        float numEdges_inv = teleportation/(float((*nodes)[i].numEdges));
	int numEdges_inv_int = *(int*)&numEdges_inv;
	float init_residual = residuals[i] * (1.0 - teleportation) * teleportation;
	//printf("node[%d] init_residual: %f\n", i, init_residual);
	int init_residual_int = *(int*)&init_residual;
        (*nodes)[i].numEdges_inv = numEdges_inv_int;
	(*nodes)[i].residual = init_residual_int;
    }

    delete [] residuals;
    fclose(f);
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
    
    
    std::ofstream out("output");
    
    // Generate and create Graph data structure
    Node *nodes, *cp_nodes;
    Edge *edges, *cp_edges;
    uint64_t numNodes = 0, numEdges = 0;

    readGraph(file, &nodes, numNodes, &edges, numEdges);
    printf("Done reading graph\n");
    printf("numNodes: %d, numEdges: %d\n", numNodes, numEdges);
    //for(int i = 0; i < numNodes; i++)
    //    printf("  nodes[%d] = {id:%d, payload:%f, edgePtr:%d, numEdges:%d, residual:%f, numEdges_inv:%f}\n", i, nodes[i].id, *(float*)&(nodes[i].payload), nodes[i].edgePtr, nodes[i].numEdges, *(float*)&(nodes[i].residual), *(float*)&(nodes[i].numEdges_inv));
    /*
    for(int i = 0; i < numEdges; i++)
        printf("  edges[%d] = {%d, %d}]\n", i, edges[i].dest, edges[i].weight);
    */
    // Generate and create Worklist data structure
    
    Job *jobs, *cp_jobs;
    uint32_t numJobs = numNodes;
    uint64_t maxJobs = (uint64_t)512*1024*1024; // 2 Giga-entries
    posix_memalign((void**)&jobs, 512, numJobs*sizeof(Job));
    
    #define INIT_IDX 0
    
    //jobs[0].priority = 0;
    //jobs[0].graphId = INIT_IDX;
    //nodes[INIT_IDX].payload = 0;
    for (int i = 0; i < numNodes; i++) {
        jobs[i].priority = 0;
	jobs[i].graphId = numNodes - i - 1;
    }
    
    uint64_t *meta, *cp_meta;
    uint32_t numMeta = 16;
    posix_memalign((void**)&meta, 512, numMeta*sizeof(uint64_t));
    
    meta[LOCK_W] = 0;
    meta[LOCK_R] = 0;
    meta[HEADPTR] = 0;
    meta[TAILPTR] = numJobs;
    meta[WLSIZE] = maxJobs;
    meta[NUMFPGA] = 3;
    meta[TAILPTR_W] = numJobs;
    meta[COMMITHEAD] = 1;
    meta[COMMITTAIL] = 0;
    meta[TOLERANCE] = *(int*)&tolerance;
    
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
    for(int i = 0; i < numNodes; i++) {
        //printf("  nodes[%d] = {%d, *%d, %d, %d}\n", i, nodes[i].id, nodes[i].payload, nodes[i].edgePtr, nodes[i].numEdges);
	int tmp = nodes[i].payload;
	float pagerank = *(float*)&tmp;
        out << nodes[i].id << "," << pagerank << "\n";
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

    //if (s == NULL) s = "./test_bc.edgelist";
    teleportation = 0.85;
    tolerance = 0.008;
    if (s == NULL) s = "./wiki-Talk.gr32";

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
    if (argc > 4) {
	fprintf (stderr, "Usage:    %s  [<teleportation>] [<tolerance>] [<input file>]\n", argv [0]);
	fprintf (stderr, "    or, set the VECTOR_SIZE environment variable\n");
	fprintf (stderr, "    or, the program uses the default size: 100\n");
	exit (1);
    }

    // ----------------
    // Do initializations of convey HW
    printf ("Initializing HW for signature %s\n", signature_name);
    bc_init_HW (signature_name);

    // ----------------
    if (argc == 4) {
        teleportation = strtof(argv[1], NULL);
	tolerance = strtof(argv[2], NULL);
        return App_SW (argv [3]);
    }
    else {
        const char* s = getenv("INPUT_FILE");
        teleportation = 0.9;
	tolerance = 0.1;
        return App_SW (s);
    }
}

#endif
