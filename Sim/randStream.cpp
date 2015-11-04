#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <cassert>
#include <time.h>
#include <stdint.h>
#include <algorithm>
#include <vector>

//#include "zsim_hooks.h"
#include "pcm.h"

PCMEvent WSMEvents[4] = {
    {"RESOURCE_STALLS.ANY", 0xA2, 0x01, "Allocator resource related stalls, including PRF, LSQ, branch mispred, etc."},
    {"RESOURCE_STALLS.LOAD", 0xA2, 0x02, "Stall cycles due to lack of load buffer entries"},
    {"RESOURCE_STALLS.RS_FULL", 0xA2, 0x04, "Stall cycles due to lack of reservation station entries"},
    //{"RESOURCE_STALLS.STORE", 0xA2, 0x08, "Stall cycles due to lack of store buffer entries"},
    //{"L1D.PEND_MISS.PENDING", 0x48, 0x01, "Increments the number of outstanding L1D misses every cycle. Set Cmask = 1 and Edge = 1 to count occurrences"}
    {"RESOURCE_STALLS.ROB_FULL", 0xA2, 0x10, "Stall cycles due to lack of ROB entries"},
    //{"RESOURCE_STALLS.FPCW", 0xA2, 0x20, "Stall cycles due to FPU control word write"},
    //{"RESOURCE_STALLS.MXCSR", 0xA2, 0x40, "Stall cycles due to MXCSR register rename occurring too close to another MXCSR rename"},
    //{"RESOURCE_STALLS.OTHER", 0xA2, 0x80, "Stall cycles due to other stalls, e.g. TAP, MOSBDrain, MS-uip"}
    /*
    {"LOAD_BLOCK.L1D", 0x03, 0x20, "Loads blocked by the L1 data cache, e.g. too many outstanding misses"},
    {"DTLB_MISSES.ANY", 0x08, 0x01, "Memory accesses that missed the DTLB"},
    {"LOAD_DISPATCH.ANY", 0x13, 0x04, "Loads dispatched from the reservation station"},
    {"BR_MISP_EXEC.ANY", 0x89, 0x7F, "Number of mispredicted near branch insts that were executed, but not necessarily retired"}
    */
};


int main(int argc, char** argv) {
    
    if(argc != 3) {
      std::cout << "./a.out <total loops> <num streams>\n";
      exit(1);
    }
    unsigned loops = atoi(argv[1]);
    unsigned streams = atoi(argv[2]);
    uint64_t nodeIdx[streams];
    uint64_t sums[streams];
    
    uint32_t arraySize = 4*1024*1024; // 64M-entries
    std::cout << "Loops: " << loops << ", Streams: " << streams << "\n";
    std::cout << "Trying to initialize array size = " << arraySize/1024 << "K entries = " << arraySize*sizeof(uint32_t)/1024 << "KB\n";

    initPCM(WSMEvents);
    
    std::vector<uint32_t> arrayTmp;
    std::vector<uint32_t> array;
    std::vector<uint32_t> counts;
    for(int i = 1; i < arraySize; i++) {
      arrayTmp.push_back(i); //rand() % arraySize;
      counts.push_back(0);
    }
    counts.push_back(0); // size = arraySize
    std::random_shuffle(arrayTmp.begin(), arrayTmp.end());

    array.resize(arraySize);
    uint32_t curIdx = 0;
    while(arrayTmp.size() > 0) {
      uint32_t newIdx = arrayTmp[arrayTmp.size()-1];
      array[curIdx] = newIdx;
      curIdx = newIdx;

      arrayTmp.pop_back();
    }
    array[curIdx] = 0;
    
    uint64_t arraySums[streams];
    uint64_t lastArray[streams];
    for(int i = 0; i < streams; i++) {
        arraySums[i] = 0;
        lastArray[i] = i; //rand() % arraySize;
    }
    std::cout << "Starting...\n";
    //zsim_roi_begin();
    SystemCounterState before_sstate = getSystemCounterState();
    for(uint64_t i = 0; i < (loops/streams)*arraySize; i++) {
        for(int j = 0; j < streams; j++) {
	    lastArray[j] = array[lastArray[j]];
	    arraySums[j] += lastArray[j];
        }
    }
    //zsim_roi_end();
    SystemCounterState after_sstate = getSystemCounterState();
    printStats(WSMEvents, before_sstate, after_sstate);
    
    std::cout << "Results: ";
    for(int i = 0; i < streams; i++) {
        std::cout << arraySums[i] << " ";
    }
    std::cout << "\n";

    /*
    std::cout << "Counts: ";
    for(int i = 0; i < arraySize; i++) {
      std::cout << counts[i] << " ";
    }
    */
}
