
#include <cstdlib>
#include <iostream>
#include <assert.h>
#include "Minnow.h"


Minnow::Minnow(uint64_t numCores, uint32_t numLocalBufs, uint32_t localBufEntries, uint32_t latency, uint32_t bucketInterval) {
    
    this->numCores = numCores;
    this->numLocalBufs = numLocalBufs;
    this->localBufEntries = localBufEntries;
    this->timestep = 0;
    this->latency = latency;
    this->bucketInterval = bucketInterval;
    
    assert((numLocalBufs == 1) || (numLocalBufs == 2));

    for(int i = 0; i < numCores; i++) {
        std::vector<uint64_t> tmp;
        std::vector<bool> tmpEn;
        for(int j = 0; j < numLocalBufs; j++) {
            tmp.push_back(j);
            tmpEn.push_back(false);
        }
        bucketPriority.push_back(tmp);
        enabled.push_back(tmpEn);
    }
    
    for(int i = 0; i < numCores; i++) {
        curWait.push_back(0);
        localHeadIdx.push_back(0);
    }
    
    for(int i = 0; i < numCores; i++) {
        std::vector<std::queue<Work> > tmp;
        std::vector<std::priority_queue<Work, std::vector<Work>, CompareTime> > tmpf;
        
        for(int j = 0; j < numLocalBufs; j++) {
            std::queue<Work> tmplw;
            std::priority_queue<Work, std::vector<Work>, CompareTime> tmpflw;
            tmp.push_back(tmplw);
            tmpf.push_back(tmpflw);
        }
        
        localWorklist.push_back(tmp);
        futureLocalWorklist.push_back(tmpf);
    }
    
    totalWrites = 0;
    iterations = 0;
    maxWrites = 0;
}

void Minnow::enqLocal(uint64_t core, uint64_t bufIdx, Work work) {
    if((localWorklist[core][bufIdx].size() + futureLocalWorklist[core][bufIdx].size()) < localBufEntries) {
        work.timestep = timestep;
        futureLocalWorklist[core][bufIdx].push(work);
        enabled[core][bufIdx] = true;
    }
    else {
        enqGlobal(work);
    }
}

void Minnow::enqGlobal(Work work) {
    work.timestep = timestep + latency;
    futureGlobalWorklist.push(work);
}

bool Minnow::localEmpty(uint64_t core, uint64_t bufIdx) {
    return localWorklist[core][bufIdx].empty() && futureLocalWorklist[core][bufIdx].empty();
}

bool Minnow::localAllEmpty(uint64_t core) {
    for(uint64_t i = 0; i < numLocalBufs; i++) {
        if(!localEmpty(core, i))
            return false;
    }
    return true;
}

uint64_t Minnow::localSize(uint64_t core, uint64_t bufIdx) {
    return localWorklist[core][bufIdx].size() + futureLocalWorklist[core][bufIdx].size();
}

uint64_t Minnow::localAllSize(uint64_t core) {
    uint64_t sum = 0;
    for(uint64_t i = 0; i < numLocalBufs; i++) {
        sum += localSize(core, i);
    }
    return sum;
}
    

/*
Try to return from highest priority local bucket (pointed to by localHeadIdx). 
If highest priority local bucket is empty, check other buckets. 
If no other bucket has work, return nothing.
If another bucket has work, set localHeadIdx to point to the next-highest priority. 
Set empty flag for all buckets between old localHeadIdx and new localHeadIdx.
Return work from localHeadIdx bucket.

If highest priority bucket is under the threshold, grab work from global worklist. 
Can grab up to N work items at a time, where all work items must be in the same bucket. 
If grabbed work items are same priority has highest priority bucket, add to bucket and do nothing. 
If grabbed work items are higher priority, change bucket priority number to new highest priority. 
If grabbed work items are lower priority, try to add to lower priority bucket. If lower priority bucket 
is full, or if work is too low priority, add back to global.

 */
bool Minnow::getWork(Work& work, uint64_t core) {
    
    uint64_t headIdx = localHeadIdx[core];
    //std::cout << "Core " << core << " getWork, headIdx: " << headIdx << "\n";
    // Find buffer to stream 
    uint64_t globalPriority = globalWorklist.empty() ? 0 : globalWorklist.top().priority;
    
    // If highest priority buffer doesn't have enough work, try to stream
    if((curWait[core] == 0) && (localSize(core, headIdx) < std::max<int>(2, latency))) {
        //std::cout << "  Try to stream, " << localSize(core, headIdx) << " < " << std::max<int>(2, latency) << "\n";
        for(uint64_t i = 0; i < numLocalBufs; i++) {
            uint64_t idx = (headIdx + i) % numLocalBufs;
            if(localEmpty(core, idx) || (!localEmpty(core, idx) && bucketPriority[core][idx] <= globalPriority)) {
                uint64_t packets = 0;
                while(!globalWorklist.empty() && 
                      (packets < std::min<int>(latency/2+1, 4)) &&
                      (localWorklist[core][idx].size() < localBufEntries) && 
                      (globalWorklist.top().priority == globalPriority)) {
                    Work newWork = globalWorklist.top();
                    globalWorklist.pop();
                    newWork.timestep += latency;
                    if(localEmpty(core, idx))
                        bucketPriority[core][idx] = newWork.priority;
                    
                    futureLocalWorklist[core][idx].push(newWork);
                    packets++;
                    //std::cout << "  Stream node " << newWork.graphId << " priority " << globalPriority << " to buf " << idx << "\n";
                }
                curWait[core] = latency;
                break;
            }
        }
    }
    if(curWait[core] > 0)
        curWait[core]--;
        
    
    // TODO: MOVE HEADIDX???
    
    // Perform get
    if(!localWorklist[core][headIdx].empty()) {
        work = localWorklist[core][headIdx].front();
        localWorklist[core][headIdx].pop();
        //std::cout << "  Got headIdx localWorklist[" << core << "][" << headIdx << "] priority " << work.priority << " node " << work.graphId <<"\n";
        return true;
    }
    else {
        for(int i = 1; i < numLocalBufs; i++) {
            uint64_t idx = (headIdx + i) % numLocalBufs;
            if(!localWorklist[core][idx].empty()) {
                work = localWorklist[core][idx].front();
                localWorklist[core][idx].pop();
                //std::cout << "  Got localWorklist[" << core << "][" << idx << "] priority " << work.priority << "\n";
                return true;
            }
        }

    }
    return false;
}

/*
If local bucket exists and is not full, add to bucket and return.
If local bucket exists and is full, push to global and return.
If local bucket does not exist, if there are no buckets with empty flags asserted, push to global and return. 
If all buckets are empty, create new bucket at highest priority and put the work item in. 
Else, if there is an empty bucket, the priority of the work item is between the higher and lower priority 
buckets, and the difference in priority between the higher priority bucket and the work item does not 
exceed some threshold, then create new bucket and put the work item in.
*/

void Minnow::putWork(Work work, uint64_t core) {
    work.priority = work.priority/bucketInterval;
    uint64_t headIdx = localHeadIdx[core];
    //std::cout << "Core " << core << " putWork priority " << work.priority << ", headIdx: " << headIdx << "\n";
    
    if(localAllEmpty(core)) {
        enqLocal(core, headIdx, work);
        bucketPriority[core][headIdx] = work.priority;
        return;
        //std::cout << "  All empty, adding to headIdx " << headIdx << "\n";
    }
    else if(bucketPriority[core][headIdx] <= work.priority) {
        enqLocal(core, headIdx, work);
        
        //std::cout << "  Adding to headIdx " << headIdx << ", bucketPriority " << bucketPriority[core][headIdx] << "->" <<work.priority <<"\n";
        // Keep head priority as high (low number) as possible
        bucketPriority[core][headIdx] = work.priority;
        return;
    }
    else {
        uint64_t priorityDiff = work.priority - bucketPriority[core][headIdx];
        for(uint64_t i = 1; i < numLocalBufs; i++) {
            uint64_t idx = (headIdx + i) % numLocalBufs;
            if(enabled[core][idx]) {
                if(bucketPriority[core][idx] <= work.priority) {
                    enqLocal(core, idx, work);
                    //std::cout << "  Adding to buf " << idx << ", bucketPriority " << bucketPriority[core][idx] << "->" <<work.priority <<"\n";
                    bucketPriority[core][idx] = work.priority;
                    return;
                }
            }
            else {
                if(work.priority - bucketPriority[core][idx] <= i) {
                    enqLocal(core, idx, work);
                    bucketPriority[core][idx] = work.priority;
                    //std::cout << "  Creating buf " << idx << ", bucketPriority " << bucketPriority[core][idx] << "\n";
                    return;
                }
            }
        }
    }
    
    // Fallthrough: add to global
    //std::cout << "  Fallthrough to Global\n";
    enqGlobal(work);
}

void Minnow::step() {
    timestep++;
    
    // step all worklists forward in time
    for(int i = 0; i < numCores; i++) {
        for(int j = 0; j < numLocalBufs; j++) {
            while(!futureLocalWorklist[i][j].empty() && futureLocalWorklist[i][j].top().timestep <= timestep) {
                localWorklist[i][j].push(futureLocalWorklist[i][j].top());
                futureLocalWorklist[i][j].pop();
        }
            /*
            if(!localWorklist[i][j].empty()) {
                bucketPriority[i][j] = localWorklist[i][j].front().priority;
            }
            */
        }
    }
    
    uint32_t val = 0;
    while(!futureGlobalWorklist.empty() && futureGlobalWorklist.top().timestep <= timestep) {
        globalWorklist.push(futureGlobalWorklist.top());
        futureGlobalWorklist.pop();
        val++;
    }
    
    if(val > maxWrites)
        maxWrites = val;
    
    totalWrites += val;
    iterations++;
    
}

bool Minnow::workAvailable(uint64_t core) {
    bool avail = false;
    for(int i = 0; i < numLocalBufs; i++) {
        if(!localWorklist[core][i].empty())
            return true;
    }
    return false;
}

bool Minnow::notEmpty() {
    bool anyNotEmpty = false;
    for(int i = 0; i < numCores; i++) {
        for(int j = 0; j < numLocalBufs; j++) {
            if(!localWorklist[i][j].empty() || !futureLocalWorklist[i][j].empty()) {
                anyNotEmpty = true;
            }
        }
    }

    return anyNotEmpty || !(globalWorklist.empty() && futureGlobalWorklist.empty());
}

uint64_t Minnow::size() {
    uint64_t size = 0;
    for(int i = 0; i < numCores; i++) {
        for(int j = 0; j < numLocalBufs; j++) {
            size += localWorklist[i][j].size();
            size += futureLocalWorklist[i][j].size();
        }
    }
    return size + globalWorklist.size() + futureGlobalWorklist.size();
}
