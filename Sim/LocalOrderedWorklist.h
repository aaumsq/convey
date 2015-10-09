#ifndef __LOCAL_ORDERED_WORKLIST__
#define __LOCAL_ORDERED_WORKLIST__

#include <queue>
#include <vector>

#include "Worklist.h"

class LocalOrderedWorklist : public Worklist {
    std::vector<std::priority_queue<Work, std::vector<Work>, ComparePriority>* > *localWorklist;
    std::vector<std::priority_queue<Work, std::vector<Work>, CompareTime>* > *futureLocalWorklist;
    std::priority_queue<Work, std::vector<Work>, ComparePriority>* globalWorklist;
    std::priority_queue<Work, std::vector<Work>, CompareTime>* futureGlobalWorklist;
    unsigned timestep;
    std::vector<uint64_t>* curPriorities;
    std::vector<uint64_t>* curWait;
    uint64_t numCores;
    uint64_t localWorkThreshold;
    uint32_t moveLatency;
    uint32_t bucketSize;
    std::vector<uint32_t>* globalWrites;
    uint64_t totalWrites;
    uint64_t iterations;
    uint32_t maxWrites;
    
public:
    LocalOrderedWorklist(uint64_t numCores, uint64_t localWorkThreshold, uint32_t latency, uint32_t bucketSize);
    virtual bool getWork(Work& work, uint64_t core);
    virtual void putWork(Work work, uint64_t core);
    virtual void step();
    virtual bool workAvailable(uint64_t core);
    virtual bool notEmpty();
    virtual uint64_t size();
};


#endif
