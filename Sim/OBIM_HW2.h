#ifndef __OBIM_HW2__
#define __OBIM_HW2__

#include <queue>
#include <vector>
#include <map>

#include "Worklist.h"
#include "UnorderedWorklist.h"
#include "RandomWorklist.h"

struct OBIMHW2Pkt {
    uint64_t timestep;
    uint64_t priority;
    Work work;
    uint64_t core;
};

struct CompareOBIMHW2 {
    bool operator()(const OBIMHW2Pkt& lhs, const OBIMHW2Pkt& rhs) {
        return lhs.timestep > rhs.timestep;
    }
};

class OBIM_HW2 : public Worklist {
    std::map<uint64_t, Worklist*> unorderedWorklists;
    std::vector<uint64_t> corePriorities;
    std::priority_queue<OBIMHW2Pkt, std::vector<OBIMHW2Pkt>, CompareOBIMHW2> futurePriorities;
    
    std::vector<uint64_t> coreIssued;

    uint64_t timestep;
    uint32_t latency;
    uint64_t numCores;
    uint32_t minBucketInterval;
    uint32_t maxBucketInterval;
    uint32_t bucketInterval;
    uint32_t bucketSize;
    uint32_t bucketThreshold;
    uint64_t goodEpochs;
    uint64_t badEpochs;
    
public:
    OBIM_HW2(uint64_t numCores, uint32_t latency, uint32_t bucketInterval, uint32_t idealBucketSize);
    virtual bool getWork(Work& work, uint64_t core);
    virtual void putWork(Work work, uint64_t core);
    virtual void step();
    virtual bool workAvailable(uint64_t core);
    virtual bool notEmpty();
    virtual uint64_t size();
};


#endif
