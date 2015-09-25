#ifndef __OBIM_HW__
#define __OBIM_HW__

#include <queue>
#include <vector>
#include <map>

#include "Worklist.h"
#include "UnorderedWorklist.h"
#include "RandomWorklist.h"

struct OBIMHWPkt {
    uint64_t timestep;
    uint64_t priority;
    Work work;
    uint64_t core;
};

struct CompareOBIMHW {
    bool operator()(const OBIMHWPkt& lhs, const OBIMHWPkt& rhs) {
        return lhs.timestep > rhs.timestep;
    }
};

class OBIM_HW : public Worklist {
    std::map<uint64_t, Worklist*> unorderedWorklists;
    std::vector<uint64_t> corePriorities;
    std::priority_queue<OBIMHWPkt, std::vector<OBIMHWPkt>, CompareOBIMHW> futurePriorities;
    
    uint64_t timestep;
    uint32_t latency;
    uint64_t numCores;
    uint32_t minBucketSize;
    uint32_t maxBucketSize;
    uint32_t bucketSize;
    uint32_t numBuckets;
    uint32_t bucketThreshold;
    
public:
    OBIM_HW(uint64_t numCores, uint32_t latency, uint32_t bucketSize, uint32_t idealNumBuckets);
    virtual bool getWork(Work& work, uint64_t core);
    virtual void putWork(Work work, uint64_t core);
    virtual void step();
    virtual bool workAvailable(uint64_t core);
    virtual bool notEmpty();
    virtual uint64_t size();
};


#endif
