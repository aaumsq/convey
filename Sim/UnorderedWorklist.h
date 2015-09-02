#ifndef __UNORDERED_WORKLIST__
#define __UNORDERED_WORKLIST__

#include <queue>

#include "Worklist.h"

class UnorderedWorklist : public Worklist {
    std::priority_queue<Work, std::vector<Work>, std::greater<Work> >* worklist;
    unsigned timestep;
    
public:
    UnorderedWorklist();
    virtual bool getWork(Work& work, uint64_t core);
    virtual void putWork(Work work, uint64_t core);
    virtual void step();
    virtual bool notEmpty();
    virtual uint64_t size();
};


#endif
