#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <cassert>
#include <time.h>
#include <queue>
#include <vector>
#include <papi.h>

#include "Graph.h"
#include "Worklist.h"

#define PERFMON

#ifdef ZSIM
#include "zsim_hooks.h"
#endif

#ifdef PERFMON
#include "pcm.h"

PCMEvent WSMEvents[4] = {
    {"RESOURCE_STALLS.ANY", 0xA2, 0x01, "Allocator resource related stalls, including PRF, LSQ, branch mispred, etc."},
    {"RESOURCE_STALLS.LOAD", 0xA2, 0x02, "Stall cycles due to lack of load buffer entries"},
    {"RESOURCE_STALLS.RS_FULL", 0xA2, 0x04, "Stall cycles due to lack of reservation station entries"},
    {"RESOURCE_STALLS.STORE", 0xA2, 0x08, "Stall cycles due to lack of store buffer entries"},
    //{"L1D.PEND_MISS.PENDING", 0x48, 0x01, "Increments the number of outstanding L1D misses every cycle. Set Cmask = 1 and Edge = 1 to count occurrences"}
    //{"RESOURCE_STALLS.ROB_FULL", 0xA2, 0x10, "Stall cycles due to lack of ROB entries"},
    //{"RESOURCE_STALLS.FPCW", 0xA2, 0x20, "Stall cycles due to FPU control word write"},
    //{"RESOURCE_STALLS.MXCSR", 0xA2, 0x40, "Stall cycles due to MXCSR register rename occurring too close to another MXCSR rename"},
    //{"RESOURCE_STALLS.OTHER", 0xA2, 0x80, "Stall cycles due to other stalls, e.g. TAP, MOSBDrain, MS-uip"}
    //{"L1D.REPL", 0x51, 0x01, "Number of lines brought into the L1D cache"},
    //{"MEM_LOAD_UOPS_RETIRED.L1D_HIT", 0xD1, 0x01, "Number of retired loads that hit the L1 data cache"},
    //{"MEM_LOAD_UOPS_RETIRED.L2_HIT", 0xD1, 0x02, "Number of retired loads that hit the L2 cache"},
    //{"MEM_LOAD_UOPS_RETIRED.LLC_MISS", 0xD1, 0x20, "Number of retired loads that missed the L3 cache"},
    //{"MEM_LOAD_UOPS_RETIRED.LLC_HIT", 0xD1, 0x04, "Number of retired loads that hit their own, unshared line in the L3 cache"},
    //{"MEM_LOAD_RETIRED.HIT_LFB", 0xCB, 0x40, "Number of retired loads that miss the L1D and the address is located in an allocated line fill buffer and will soon be committed to cache"},
    //{"MEM_LOAD_RETIRED.DTLB_MISS", 0xCB, 0x80, "Number of retired loads that missed the DTLB"},
    //{"LOAD_BLOCK.L1D", 0x03, 0x20, "Loads blocked by the L1 data cache, e.g. too many outstanding misses"},
    //{"DTLB_MISSES.ANY", 0x08, 0x01, "Memory accesses that missed the DTLB"},
    //{"LOAD_DISPATCH.ANY", 0x13, 0x04, "Loads dispatched from the reservation station"},
    //{"BR_MISP_EXEC.ANY", 0x89, 0x7F, "Number of mispredicted near branch insts that were executed, but not necessarily retired"},
};

#endif



int main(int argc, char** argv) {
    
    if((argc != 3) && (argc != 5)) {
        std::cout << "ERROR: incorrect input parameters!\n";
        std::cout << argv[0] << " <input file name> <source vertex>\n-- OR --\n";
        std::cout << argv[0] << " <input file name> <source vertex> -out <output file name>" << std::endl;
        exit(1);
    }
    
    unsigned source = atoi(argv[2]);
    bool genOutput = false;
    std::ofstream out(argv[4]);
    if(argc == 5) {
        genOutput = true;
    }

    
#ifdef PERFMON
    initPCM(WSMEvents);
#endif


    Graph* graph = new Graph();
    std::priority_queue<Work, std::vector<Work>, ComparePriority> wl;
    
    std::cout << "Running on " << argv[1] << " with source vertex " << source << std::endl;
    time_t t1, t2, t3;
    t1 = time(NULL);
    
    graph->loadEdgelistFile(argv[1]);
    
    std::cout << "Done loading\n";

    Work initWork = {source, 0, 0};
    wl.push(initWork);
    graph->getNode(source)->payload = 0;

    
    t2 = time(NULL);
    
        
#ifdef PERFMON
    SystemCounterState before_sstate = getSystemCounterState();
#endif

    Work work;
    
    while(!wl.empty()) {
        work = wl.top();
        wl.pop();
        Node* curNode = graph->getNode(work.graphId);
        for(int i = 0; i < curNode->numEdges; i++) {
            Edge* edge = graph->getEdge(curNode->edgePtr + i);
            Node* destNode = graph->getNode(edge->dest);
            if(curNode->payload + edge->weight < destNode->payload) {
                destNode->payload = curNode->payload + edge->weight;
                Work newWork = {edge->dest, destNode->payload/50000, 0};
                wl.push(newWork);
            }
        }
    }
    
#ifdef PERFMON
    SystemCounterState after_sstate = getSystemCounterState();
    printStats(WSMEvents, before_sstate, after_sstate);
#endif

    
    t3 = time(NULL);
    
    std::cout << "Setup time: " << (t2 - t1) << " seconds\n";
    std::cout << "SSSP time: " << (t3 - t2) << " seconds\n";
    
    
    if(genOutput) {
        for(int i = 0; i < graph->numNodes; i++) {
            out << graph->getNode(i)->id << "," << graph->getNode(i)->payload << "\n";
        }
        out.close();
    }
}
