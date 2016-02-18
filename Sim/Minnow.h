#ifndef __MINNOW__
#define __MINNOW__

#include <queue>
#include <vector>

#include "Worklist.h"

class Minnow : public Worklist {
    std::vector<std::vector<std::queue<Work> > > localWorklist;
    std::vector<std::vector<std::priority_queue<Work, std::vector<Work>, CompareTime> > > futureLocalWorklist;
    std::priority_queue<Work, std::vector<Work>, ComparePriority> globalWorklist;
    std::priority_queue<Work, std::vector<Work>, CompareTime> futureGlobalWorklist;
    unsigned timestep;
    std::vector<std::vector<uint64_t> > bucketPriority;
    std::vector<std::vector<bool> > enabled;
    std::vector<uint64_t> localHeadIdx;
    std::vector<uint64_t> curWait;
    uint64_t numCores;
    uint32_t numLocalBufs, localBufEntries;
    uint32_t latency;
    uint32_t bucketInterval;
    uint64_t totalWrites;
    uint64_t iterations;
    uint32_t maxWrites;

    void enqLocal(uint64_t core, uint64_t bufIdx, Work work);
    void enqGlobal(Work work);
    bool localAllEmpty(uint64_t core);
    bool localEmpty(uint64_t core, uint64_t bufIdx);
    uint64_t localAllSize(uint64_t core);
    uint64_t localSize(uint64_t core, uint64_t bufIdx);
    
public:
    Minnow(uint64_t numCores, uint32_t numLocalBufs, uint32_t localBufEntries, uint32_t latency, uint32_t bucketInterval);
    virtual bool getWork(Work& work, uint64_t core);
    virtual void putWork(Work work, uint64_t core);
    virtual void step();
    virtual bool workAvailable(uint64_t core);
    virtual bool notEmpty();
    virtual uint64_t size();
};


#endif
