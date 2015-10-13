#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <cassert>
#include <time.h>
#include <queue>
#include <vector>

#include <cpucounters.h>

#include "Graph.h"
#include "Worklist.h"

struct PCMEvent {
    const char * name;
    unsigned char event;
    unsigned char umask;
    const char * description;
};

PCMEvent WSMEvents[4] = {
    {"RESOURCE_STALLS.ANY", 0xA2, 0x01, "Allocator resource related stalls, including PRF, LSQ, branch mispred, etc."},
    {"RESOURCE_STALLS.LOAD", 0xA2, 0x02, "Stall cycles due to lack of load buffer entries"},
    {"RESOURCE_STALLS.RS_FULL", 0xA2, 0x04, "Stall cycles due to lack of reservation station entries"},
    //{"L1D.PEND_MISS.PENDING", 0x48, 0x01, "Increments the number of outstanding L1D misses every cycle. Set Cmask = 1 and Edge = 1 to count occurrences"}
    {"RESOURCE_STALLS.ROB_FULL", 0xA2, 0x10, "Stall cycles due to lack of ROB entries"}
    /*
    {"LOAD_BLOCK.L1D", 0x03, 0x20, "Loads blocked by the L1 data cache, e.g. too many outstanding misses"},
    {"DTLB_MISSES.ANY", 0x08, 0x01, "Memory accesses that missed the DTLB"},
    {"LOAD_DISPATCH.ANY", 0x13, 0x04, "Loads dispatched from the reservation station"},
    {"BR_MISP_EXEC.ANY", 0x89, 0x7F, "Number of mispredicted near branch insts that were executed, but not necessarily retired"}
    */
};

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
    uint64_t iters = 0;
    uint64_t workPerCurIter = 0;
    uint64_t workGenPerCurIter = 0;
    uint64_t conflictsPerCurIter = 0;
    uint64_t totalWork = 0;
    uint64_t totalWorkIssued = 0;
    uint64_t totalWorkGen = 0;
    uint64_t totalConflicts = 0;
    bool infCores = false;
    uint64_t maxCores = 128;
    uint64_t maxWork = 0;
    
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
    
    PCM * m = PCM::getInstance();
    
    PCM::ExtendedCustomCoreEventDescription conf;
    conf.fixedCfg = NULL; // default
    conf.nGPCounters = 4;
    EventSelectRegister regs[4];
    conf.gpCounterCfg = regs;
    EventSelectRegister def_event_select_reg;
    def_event_select_reg.value = 0;
    def_event_select_reg.fields.usr = 1;
    def_event_select_reg.fields.os = 1;
    def_event_select_reg.fields.enable = 1;
    for(int i=0;i<4;++i)
        regs[i] = def_event_select_reg;
    
    for(int i = 0; i < 4; i++) {
        regs[i].fields.event_select = WSMEvents[i].event;
        regs[i].fields.umask = WSMEvents[i].umask;
    }
    
    PCM::ErrorCode status = m->program(PCM::EXT_CUSTOM_CORE_EVENTS, &conf);
    switch(status) {
    case PCM::Success:
        break;
    case PCM::MSRAccessDenied:
        std::cerr << "Access to Intel(r) Performance Counter Monitor has denied (no MSR or PCI CFG space access).\n";
        exit(EXIT_FAILURE);
    case PCM::PMUBusy:
        std::cerr << "Access to Intel(r) Performance Counter Monitor has denied (Performance Monitoring Unit is occupied by other application). Try to stop the application that uses PMU.\n";
        std::cerr << "Alternatively you can try to reset PMU configuration at your own risk. Try to reset? (y/n)\n";
        char yn;
        std::cin >> yn;
        if ('y' == yn) {
            m->resetPMU();
            std::cerr << "PMU configuration has been reset. Try to rerun the program again.\n";
        }
        exit(EXIT_FAILURE);
    default:
        std::cerr << "Access to Intel(r) Performance Counter Monitor has denied (Unknown error).\n";
        exit(EXIT_FAILURE);
    }
    
    std::cerr << "\nDetected "<< m->getCPUBrandString() << " \"Intel(r) microarchitecture codename "<<m->getUArchCodename()<<"\"\n";
    
    
    unsigned num = 10;
    uint64_t nodeIdx[num];
    uint64_t sums[num];
    Node* curNodes[num];
    Edge* edges[num];
    
    std::cout << "Inputs: ";
    for(int i = 0; i < num; i++) {
        nodeIdx[i] = source+i;
        sums[i] = 0;
        std::cout << nodeIdx[i] << " ";
    }
    std::cout << "\n";

    std::cout << "Trying to initialize array\n";
    
    uint32_t arraySize = 128*1024*1024; // 64M-entries
    uint32_t* array = new uint32_t[arraySize]();
    for(int i = 0; i < arraySize; i++) {
        array[i] = rand() % arraySize;
    }
    uint64_t arraySums[num];
    uint64_t lastArray[num];
    for(int i = 0; i < num; i++) {
        arraySums[i] = 0;
        lastArray[i] = rand() % arraySize;
    }
    std::cout << "Starting...\n";

    SystemCounterState before_sstate = getSystemCounterState();
    
    for(uint64_t i = 0; i < 10000000; i++) {
        for(int j = 0; j < num; j++) {
            lastArray[j] = array[lastArray[j]];
            arraySums[j] += lastArray[j];
        }
    }
    
    /*
    for(uint64_t i = 0; i < 100000000; i++) {
        for(int j = 0; j < num; j++) {
            curNodes[j] = graph->getNode(nodeIdx[j]);
        }
        for(int j = 0; j < num; j++) {
            edges[j] = graph->getEdge(curNodes[j]->edgePtr);
        }
        for(int j = 0; j < num; j++) {
            sums[j] += edges[j]->weight;
            nodeIdx[j] = edges[j]->dest;
        }
    }    
    */
    SystemCounterState after_sstate = getSystemCounterState();
    
    
    std::cout << "Results: ";
    for(int i = 0; i < num; i++) {
        std::cout << arraySums[i] << " ";
    }
    std::cout << "\n";
    
    uint64_t insts = getInstructionsRetired(before_sstate, after_sstate);
    uint64_t cycles = getCycles(before_sstate, after_sstate);

    for(int i = 0; i < 4; i++) {
        uint64_t counter = getNumberOfCustomEvents(i, before_sstate, after_sstate);
        std::cout << WSMEvents[i].name << ": " << counter 
                  << ", PKI: " << double(counter)*1000.0/double(insts) 
                  << ", PKC: " << double(counter)*1000.0/double(cycles) << "\n";
    }
    
    std::cout << "Instructions retired: " << insts << "\n";
    std::cout << "Cycles: " << cycles << "\n";
    std::cout << "IPC: " << getIPC(before_sstate, after_sstate) << "\n";
    std::cout << "Calc IPC: " << double(insts)/double(cycles) << "\n";
    /*
    std::cout << "Instructions per clock: " << getIPC(before_sstate,after_sstate)
              << "\nL3 cache hit ratio: " << getL3CacheHitRatio(before_sstate,after_sstate)
              << "\nBytes read: " << getBytesReadFromMC(before_sstate,after_sstate)
              << "\n";
    */
    t3 = time(NULL);
    m->cleanup();
    
    std::cout << "Setup time: " << (t2 - t1) << " seconds\n";
    std::cout << "SSSP time: " << (t3 - t2) << " seconds\n";
    
    
    std::cout << "Iters: " << iters << ", totalWork: " << totalWork << ", max cores active: " << maxWork 
              << ", utilization: " << double(totalWorkIssued)/double(iters*maxCores) 
              << ", executed: " << double(totalWork)/double(iters*maxCores) 
              << ", conflict percent: " << double(totalConflicts)/double(iters*maxCores) << std::endl;
    
    if(genOutput) {
        for(int i = 0; i < graph->numNodes; i++) {
            out << graph->getNode(i)->id << "," << graph->getNode(i)->payload << "\n";
        }
        out.close();
    }
    
}
