#ifndef __RANDOM_WORKLIST__
#define __RANDOM_WORKLIST__

#include <deque>
#include <queue>

#include "Worklist.h"


class RandomWorklist : public Worklist {
    std::deque<Work>* worklist;
    std::priority_queue<Work, std::vector<Work>, CompareTime>* futureWorklist;
    uint64_t timestep;
    unsigned latency;
    
public:
    RandomWorklist(unsigned latency);
    virtual bool getWork(Work& work, uint64_t core);
    virtual void putWork(Work work, uint64_t core);
    virtual void step();
    virtual bool workAvailable(uint64_t core);
    virtual bool notEmpty();
    virtual uint64_t size();
};


#endif
