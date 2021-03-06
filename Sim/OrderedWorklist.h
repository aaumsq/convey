#ifndef __ORDERED_WORKLIST__
#define __ORDERED_WORKLIST__

#include <queue>

#include "Worklist.h"


class OrderedWorklist : public Worklist {
    std::priority_queue<Work, std::vector<Work>, ComparePriority>* worklist;
    std::priority_queue<Work, std::vector<Work>, CompareTime>* futureWorklist;
    uint64_t timestep;
    unsigned latency;
    uint64_t bucketSize;
    
public:
    OrderedWorklist(unsigned latency, uint64_t bucketSize);
    virtual bool getWork(Work& work, uint64_t core);
    virtual void putWork(Work work, uint64_t core);
    virtual void step();
    virtual bool workAvailable(uint64_t core);
    virtual bool notEmpty();
    virtual uint64_t size();
};


#endif
