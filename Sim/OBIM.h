#ifndef __OBIM__
#define __OBIM__

#include <queue>
#include <vector>
#include <map>

#include "Worklist.h"
#include "UnorderedWorklist.h"

class OBIM : public Worklist {
    std::map<uint64_t, Worklist*> unorderedWorklists;
    std::vector<uint64_t> corePriorities;
    
    unsigned timestep;
    uint32_t latency;
    std::vector<uint64_t>* curPriorities;
    uint64_t numCores;
    
public:
    OBIM(uint64_t numCores, uint32_t latency);
    virtual bool getWork(Work& work, uint64_t core);
    virtual void putWork(Work work, uint64_t core);
    virtual void step();
    virtual bool workAvailable(uint64_t core);
    virtual bool notEmpty();
    virtual uint64_t size();
};


#endif
