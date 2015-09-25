#ifndef __LOCAL_UNORDERED_WORKLIST__
#define __LOCAL_UNORDERED_WORKLIST__

#include <queue>
#include <vector>

#include "Worklist.h"

class LocalUnorderedWorklist : public Worklist {
    std::vector<std::queue<Work>* > *localWorklist;
    std::vector<std::priority_queue<Work, std::vector<Work>, CompareTime>* > *futureLocalWorklist;
    std::queue<Work>* globalWorklist;
    std::priority_queue<Work, std::vector<Work>, CompareTime>* futureGlobalWorklist;
    unsigned timestep;
    uint64_t numCores;
    uint64_t localWorkThreshold;
    uint32_t latency;
    uint64_t packetSize;
    
public:
    LocalUnorderedWorklist(uint64_t numCores, uint64_t localWorkThreshold, uint64_t packetSize, uint32_t latency);
    virtual bool getWork(Work& work, uint64_t core);
    virtual void putWork(Work work, uint64_t core);
    virtual void step();
    virtual bool workAvailable(uint64_t core);
    virtual bool notEmpty();
    virtual uint64_t size();
};


#endif
