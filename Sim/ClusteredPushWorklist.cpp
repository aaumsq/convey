
#include <cstdlib>
#include <iostream>
#include <assert.h>
#include "ClusteredPushWorklist.h"


ClusteredPushWorklist::ClusteredPushWorklist(uint64_t numCores, uint32_t clusterSize, uint32_t latency) {
    
    this->numCores = numCores;
    this->clusterSize = clusterSize;
    this->timestep = 0;
    this->latency = latency;
    this->packetSize = packetSize;
    
    localWorklist = new std::vector<std::queue<Work>* >();
    futureLocalWorklist = new std::vector<std::priority_queue<Work, std::vector<Work>, CompareTime>* >();
    
    assert(numCores % clusterSize == 0);
    for(int i = 0; i < numCores/clusterSize; i++) {
        std::queue<Work>* tmplw = new std::queue<Work>();
        std::priority_queue<Work, std::vector<Work>, CompareTime>* tmpflw = new std::priority_queue<Work, std::vector<Work>, CompareTime>();
        localWorklist->push_back(tmplw);
        futureLocalWorklist->push_back(tmpflw);
    }
    
}

bool ClusteredPushWorklist::getWork(Work& work, uint64_t core) {
    uint64_t cluster = core / clusterSize;
    
    if(localWorklist->at(cluster)->empty()) {
        // Do nothing???
        return false;
    }
    
    work = localWorklist->at(cluster)->front();
    localWorklist->at(cluster)->pop();
    return true;
}

void ClusteredPushWorklist::putWork(Work work, uint64_t core) {
    // Push to random cluster worklist
    uint64_t randCore = rand() % numCores;
    uint64_t cluster = randCore / clusterSize;
    
    futureLocalWorklist->at(cluster)->push(work);
    //std::cout << "Core " << core << " pushing packet to local, size = " << size << "\n";
}

void ClusteredPushWorklist::step() {
    timestep++;
    
    // step all worklists forward in time
    for(int i = 0; i < numCores/clusterSize; i++) {
        while(!futureLocalWorklist->at(i)->empty() && futureLocalWorklist->at(i)->top().timestep <= timestep) {
            localWorklist->at(i)->push(futureLocalWorklist->at(i)->top());
            futureLocalWorklist->at(i)->pop();
        }       
    }
}

bool ClusteredPushWorklist::workAvailable(uint64_t core) {
    
    bool anyNotEmpty = false;
    for(int i = 0; i < numCores/clusterSize; i++) {
        if(!localWorklist->at(i)->empty())
            anyNotEmpty = true;
    }
    return anyNotEmpty;
}

bool ClusteredPushWorklist::notEmpty() {
    bool anyNotEmpty = false;
    for(int i = 0; i < numCores/clusterSize; i++) {
        if(!localWorklist->at(i)->empty() || !futureLocalWorklist->at(i)->empty()) {
            anyNotEmpty = true;
        }
    }

    return anyNotEmpty;
}

uint64_t ClusteredPushWorklist::size() {
    uint64_t size = 0;
    for(int i = 0; i < numCores/clusterSize; i++) {
        size += localWorklist->at(i)->size();
        size += futureLocalWorklist->at(i)->size();
    }
    return size;
}
