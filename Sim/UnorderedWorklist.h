#ifndef __UNORDERED_WORKLIST__
#define __UNORDERED_WORKLIST__

#include <queue>

#include "Worklist.h"

class UnorderedWorklist : public Worklist {
    std::queue<Work>* worklist;
    std::priority_queue<Work, std::vector<Work>, CompareTime>* futureWorklist;
    unsigned timestep;
    unsigned latency;

public:
    UnorderedWorklist(unsigned latency);
    virtual bool getWork(Work& work, uint64_t core);
    virtual void putWork(Work work, uint64_t core);
    virtual void step();
    virtual bool workAvailable(uint64_t core);
    virtual bool notEmpty();
    virtual uint64_t size();
};


#endif
