
#include <cstdlib>
#include <iostream>
#include "LocalUnorderedWorklist.h"


LocalUnorderedWorklist::LocalUnorderedWorklist(uint64_t numCores, uint64_t localWorkThres, uint64_t packetSize, uint32_t latency) {
    
    this->numCores = numCores;
    this->localWorkThreshold = localWorkThres;
    this->timestep = 0;
    this->latency = latency;
    this->packetSize = packetSize;
    
    localWorklist = new std::vector<std::queue<Work>* >();
    futureLocalWorklist = new std::vector<std::priority_queue<Work, std::vector<Work>, CompareTime>* >();
    
    for(int i = 0; i < numCores; i++) {
        std::queue<Work>* tmplw = new std::queue<Work>();
        std::priority_queue<Work, std::vector<Work>, CompareTime>* tmpflw = new std::priority_queue<Work, std::vector<Work>, CompareTime>();
        localWorklist->push_back(tmplw);
        futureLocalWorklist->push_back(tmpflw);
    }
    
    globalWorklist = new std::queue<Work>();
    futureGlobalWorklist = new std::priority_queue<Work, std::vector<Work>, CompareTime>();

}

bool LocalUnorderedWorklist::getWork(Work& work, uint64_t core) {
    if(localWorklist->at(core)->empty()) {
        
        // Randomly choose a worklist with work
        bool found = false;
        uint32_t randCore = 0;
        
        for(uint32_t i = 0; i < numCores; i++) {
            if(localWorklist->at(i)->size() > 2) {
                randCore = i;
                //std::cout << "Found " << randCore << std::endl;        
            }
        }
        
        
        // request data from global worklist
        int packets = 0;
        //std::cout << "Empty, getting packets from global: " << globalWorklist->size() << std::endl;
        while(!localWorklist->at(randCore)->empty() && (packets < packetSize)) {
            Work newWork = localWorklist->at(randCore)->front();
            newWork.timestep += latency;
            futureLocalWorklist->at(core)->push(newWork);
            localWorklist->at(randCore)->pop();
            packets++;
        }
        
        return false;
    }
    
    work = localWorklist->at(core)->front();
    localWorklist->at(core)->pop();
    return true;
}

void LocalUnorderedWorklist::putWork(Work work, uint64_t core) {
    uint64_t size = localWorklist->at(core)->size();
    
    futureLocalWorklist->at(core)->push(work);
    //std::cout << "Core " << core << " pushing packet to local, size = " << size << "\n";
}

void LocalUnorderedWorklist::step() {
    timestep++;
    
    // step all worklists forward in time
    for(int i = 0; i < numCores; i++) {
        while(!futureLocalWorklist->at(i)->empty() && futureLocalWorklist->at(i)->top().timestep <= timestep) {
            localWorklist->at(i)->push(futureLocalWorklist->at(i)->top());
            futureLocalWorklist->at(i)->pop();
        }       
    }
    
    while(!futureGlobalWorklist->empty() && futureGlobalWorklist->top().timestep <= timestep) {
        globalWorklist->push(futureGlobalWorklist->top());
        futureGlobalWorklist->pop();
    }    
}

bool LocalUnorderedWorklist::workAvailable(uint64_t core) {
    
    bool anyNotEmpty = false;
    for(int i = 0; i < numCores; i++) {
        if(!localWorklist->at(i)->empty())
            anyNotEmpty = true;
    }
    return anyNotEmpty;
    
    //return !localWorklist->at(core)->empty();
}

bool LocalUnorderedWorklist::notEmpty() {
    bool anyNotEmpty = false;
    for(int i = 0; i < numCores; i++) {
        if(!localWorklist->at(i)->empty() || !futureLocalWorklist->at(i)->empty()) {
            anyNotEmpty = true;
        }
    }

    return anyNotEmpty || !(globalWorklist->empty() && futureGlobalWorklist->empty());
}

uint64_t LocalUnorderedWorklist::size() {
    uint64_t size = 0;
    for(int i = 0; i < numCores; i++) {
        size += localWorklist->at(i)->size();
        size += futureLocalWorklist->at(i)->size();
    }
    return size + globalWorklist->size() + futureGlobalWorklist->size();
}
