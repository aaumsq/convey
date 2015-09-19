#ifndef __LIFO__
#define __LIFO__

#include <queue>
#include <stack>

#include "Worklist.h"

class LIFO : public Worklist {
    std::stack<Work>* worklist;
    std::priority_queue<Work, std::vector<Work>, CompareTime>* futureWorklist;
    unsigned timestep;
    unsigned latency;

public:
    LIFO(unsigned latency);
    ~LIFO();
    virtual bool getWork(Work& work, uint64_t core);
    virtual void putWork(Work work, uint64_t core);
    virtual void step();
    virtual bool workAvailable(uint64_t core);
    virtual bool notEmpty();
    virtual uint64_t size();
};


#endif
