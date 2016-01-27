#ifndef __CLUSTERED_PUSH_WORKLIST__
#define __CLUSTERED_PUSH_WORKLIST__

#include <queue>
#include <vector>

#include "Worklist.h"

class ClusteredPushWorklist : public Worklist {
    std::vector<std::queue<Work>* > *localWorklist;
    std::vector<std::priority_queue<Work, std::vector<Work>, CompareTime>* > *futureLocalWorklist;
    unsigned timestep;
    uint64_t numCores;
    uint32_t clusterSize;
    uint32_t latency;
    uint64_t packetSize;
    
public:
    ClusteredPushWorklist(uint64_t numCores, uint32_t clusterSize, uint32_t latency);
    virtual bool getWork(Work& work, uint64_t core);
    virtual void putWork(Work work, uint64_t core);
    virtual void step();
    virtual bool workAvailable(uint64_t core);
    virtual bool notEmpty();
    virtual uint64_t size();
};


#endif
