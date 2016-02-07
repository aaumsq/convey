
#include <cstdlib>
#include <iostream>
#include "LocalOrderedWorklist.h"


LocalOrderedWorklist::LocalOrderedWorklist(uint64_t numCores, uint64_t localWorkThres, uint32_t latency, uint32_t bucketSize) {
    
    this->numCores = numCores;
    this->localWorkThreshold = localWorkThres;
    this->timestep = 0;
    this->moveLatency = latency;
    this->bucketSize = bucketSize;

    curPriorities = new std::vector<uint64_t>();
    for(int i = 0; i < numCores; i++) {
        curPriorities->push_back(0);
    }
    
    curWait = new std::vector<uint64_t>();
    for(int i = 0; i < numCores; i++) {
        curWait->push_back(0);
    }

    localWorklist = new std::vector<std::priority_queue<Work, std::vector<Work>, ComparePriority>* >();
    futureLocalWorklist = new std::vector<std::priority_queue<Work, std::vector<Work>, CompareTime>* >();
    
    for(int i = 0; i < numCores; i++) {
        std::priority_queue<Work, std::vector<Work>, ComparePriority>* tmplw = new std::priority_queue<Work, std::vector<Work>, ComparePriority>();
        std::priority_queue<Work, std::vector<Work>, CompareTime>* tmpflw = new std::priority_queue<Work, std::vector<Work>, CompareTime>();
        localWorklist->push_back(tmplw);
        futureLocalWorklist->push_back(tmpflw);
    }
    
    globalWorklist = new std::priority_queue<Work, std::vector<Work>, ComparePriority>();
    futureGlobalWorklist = new std::priority_queue<Work, std::vector<Work>, CompareTime>();
    
    globalWrites = new std::vector<uint32_t>();
    totalWrites = 0;
    iterations = 0;
    maxWrites = 0;
}

bool LocalOrderedWorklist::getWork(Work& work, uint64_t core) {
    if((curWait->at(core) == 0) && (localWorklist->at(core)->size() < std::max<int>(2, moveLatency))) {
      
        // request data from global worklist
        int packets = 0;
        uint64_t globalPriority = globalWorklist->empty() ? 0 : globalWorklist->top().priority;
        
        while(!globalWorklist->empty() && (packets < std::min<int>(moveLatency/2+1, 4)) && (globalWorklist->top().priority == globalPriority)) {
            Work newWork = globalWorklist->top();
            newWork.timestep += moveLatency;
            futureLocalWorklist->at(core)->push(newWork);
            globalWorklist->pop();
            packets++;
        }
        curWait->at(core) = moveLatency;
        
        //return false;
    }
    if(curWait->at(core) > 0)
        curWait->at(core)--;
    
    //if((localWorklist->at(core)->size() > 0) && (localWorklist->at(core)->top().priority == curPriorities->at(core))) {
    if(localWorklist->at(core)->size() > 0) {
        work = localWorklist->at(core)->top();
        localWorklist->at(core)->pop();
        return true;
    }
    
    return false;
}

void LocalOrderedWorklist::putWork(Work work, uint64_t core) {
    uint64_t size = localWorklist->at(core)->size();
    //uint64_t size = localWorklist->at(core)->size() + futureLocalWorklist->at(core)->size();
    //bool push = (rand() % localWorkThreshold)*6 < size;
    work.priority = work.priority/bucketSize;
    uint64_t minPriority = (size==0) ? 0 : localWorklist->at(core)->top().priority;
    
    bool push = (work.priority > minPriority);
    
    // Push to global
    if(push || (size >= localWorkThreshold)) {
        work.timestep = timestep + moveLatency;
        futureGlobalWorklist->push(work);
    }
    // Push to local
    else {
        work.timestep = timestep;
        futureLocalWorklist->at(core)->push(work);
    }
}

void LocalOrderedWorklist::step() {
    timestep++;
    
    // step all worklists forward in time
    for(int i = 0; i < numCores; i++) {
        while(!futureLocalWorklist->at(i)->empty() && futureLocalWorklist->at(i)->top().timestep <= timestep) {
            localWorklist->at(i)->push(futureLocalWorklist->at(i)->top());
            futureLocalWorklist->at(i)->pop();
        }
       
        if(!localWorklist->at(i)->empty()) {
            curPriorities->at(i) = localWorklist->at(i)->top().priority;
        }
    }
    
    uint32_t val = 0;
    while(!futureGlobalWorklist->empty() && futureGlobalWorklist->top().timestep <= timestep) {
        globalWorklist->push(futureGlobalWorklist->top());
        futureGlobalWorklist->pop();
        val++;
    }
    
    if(val > maxWrites)
        maxWrites = val;
    
    globalWrites->push_back(val);
    totalWrites += val;
    iterations++;
    //std::cout << "Wrote " << val << " entries, avg = " << double(totalWrites)/double(iterations) << ", max = " << maxWrites << std::endl;
    /*
    // Calculate stats
    uint64_t minPriority = -1;
    uint64_t minPriorityIdx = -1;
    uint64_t maxPriority = 0;
    uint64_t maxPriorityIdx = -1;
    for(int i = 0; i < numCores; i++) {
        if(!localWorklist->at(i)->empty()) {
            if(localWorklist->at(i)->top().priority < minPriority) {
                minPriority = localWorklist->at(i)->top().priority;
                minPriorityIdx = i;
            }
            if(localWorklist->at(i)->top().priority > maxPriority) {
                maxPriority = localWorklist->at(i)->top().priority;
                maxPriorityIdx = i;
            }
        }
    }
    
    uint64_t globalPriority = -1;
    if(!globalWorklist->empty()) {
        globalPriority = globalWorklist->top().priority;
    }
    
    std::vector<uint64_t> priorityDiff;
    uint64_t activeCores = 0;
    uint64_t totalDiff = 0;
    for(int i = 0; i < numCores; i++) {
        if(!localWorklist->at(i)->empty()) {
            Work w = localWorklist->at(i)->top();
            priorityDiff.push_back(w.priority - minPriority);
            totalDiff += w.priority - minPriority;
            activeCores++;
        }
        else {
            priorityDiff.push_back(0);
        }
    }

    uint64_t totalLocalSize = 0;
    for(int i = 0; i < numCores; i++) {
        if(!localWorklist->at(i)->empty()) {
            totalLocalSize += localWorklist->at(i)->size();
        }
    }
    
    //std::cout.precision(5);
    //std::cout << "Average local worklist size: " << double(totalLocalSize)/double(numCores) << ", average active local worklist size: " << double(totalLocalSize)/double(activeCores) << ", global worklist size: " << globalWorklist->size() << std::endl;
    //std::cout << "min priority: "<< minPriority << ", max priority: " << maxPriority << ", totalDiff: " << 100.0*double(totalDiff)/double(minPriority) <<  ", avgDiff: " << 100.0*double(totalDiff)/(double(activeCores) * double(minPriority)) << "%\n";
    */
    
}

bool LocalOrderedWorklist::workAvailable(uint64_t core) {
    return !localWorklist->at(core)->empty();
}

bool LocalOrderedWorklist::notEmpty() {
    bool anyNotEmpty = false;
    for(int i = 0; i < numCores; i++) {
        if(!localWorklist->at(i)->empty() || !futureLocalWorklist->at(i)->empty()) {
            anyNotEmpty = true;
        }
    }

    return anyNotEmpty || !(globalWorklist->empty() && futureGlobalWorklist->empty());
}

uint64_t LocalOrderedWorklist::size() {
    uint64_t size = 0;
    for(int i = 0; i < numCores; i++) {
        size += localWorklist->at(i)->size();
        size += futureLocalWorklist->at(i)->size();
    }
    return size + globalWorklist->size() + futureGlobalWorklist->size();
}
