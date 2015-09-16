#ifndef __OBIM__
#define __OBIM__

#include <queue>
#include <vector>
#include <map>

#include "Worklist.h"
#include "UnorderedWorklist.h"

struct OBIMPkt {
    uint64_t timestep;
    uint64_t priority;
    Work work;
    uint64_t core;
};

struct CompareOBIM {
    bool operator()(const OBIMPkt& lhs, const OBIMPkt& rhs) {
        return lhs.timestep > rhs.timestep;
    }
};

class OBIM : public Worklist {
    std::map<uint64_t, UnorderedWorklist*> unorderedWorklists;
    std::vector<uint64_t> corePriorities;
    std::priority_queue<OBIMPkt, std::vector<OBIMPkt>, CompareOBIM> futurePriorities;
    
    uint64_t timestep;
    uint32_t latency;
    uint64_t numCores;
    uint32_t bucketSize;
    
public:
    OBIM(uint64_t numCores, uint32_t latency, uint32_t bucketSize);
    virtual bool getWork(Work& work, uint64_t core);
    virtual void putWork(Work work, uint64_t core);
    virtual void step();
    virtual bool workAvailable(uint64_t core);
    virtual bool notEmpty();
    virtual uint64_t size();
};


#endif
