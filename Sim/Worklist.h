#ifndef __WORKLIST__
#define __WORKLIST__

#include <stdint.h>

struct Work {
    uint64_t graphId;
    uint64_t priority;
    uint64_t timestep;
    
    bool operator>(const Work& rhs) const {
        if(timestep != rhs.timestep)
            return timestep > rhs.timestep;
        else
            return priority > rhs.priority;
    }
};

struct CompareTime {
    bool operator()(const Work& lhs, const Work& rhs) {
        return lhs.timestep > rhs.timestep;
    }
};

struct ComparePriority {
    bool operator()(const Work& lhs, const Work& rhs) {
        return lhs.priority > rhs.priority;
    }
};


class Worklist {
public:
    virtual bool getWork(Work& work, uint64_t core) = 0;
    virtual void putWork(Work work, uint64_t core) = 0;
    virtual void step() = 0;
    virtual bool workAvailable(uint64_t core) = 0;
    virtual bool notEmpty() = 0;
    virtual uint64_t size() = 0;
};


#endif
