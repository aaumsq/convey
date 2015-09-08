
#include <cstdlib>
#include <iostream>
#include "OBIM.h"


OBIM::OBIM(uint64_t numCores, uint32_t latency) {
  
    this->numCores = numCores;
    this->latency = latency;
    this->timestep = 0;
  
    corePriorities.resize(numCores);
    for(int i = 0; i < corePriorities.size(); i++) {
        corePriorities[i] = 0;
    }
}

bool OBIM::getWork(Work& work, uint64_t core) {
    uint64_t priority = corePriorities[core];
    //std::cout << "GetWork core " << core << " priority " << priority << "\n";
    bool success = false;
    bool found = false;
    if(unorderedWorklists.count(priority) > 0) {
        found = true;
        success = unorderedWorklists[priority]->getWork(work, core);
    }
  
    // If empty, get current best priority
    if(!success) {
        if(found && !unorderedWorklists[priority]->notEmpty()) {
            unorderedWorklists.erase(priority);
        }
        
        // Find highest priority that has available work right now
        std::map<uint64_t, Worklist*>::iterator it = unorderedWorklists.begin();
        while((it!=unorderedWorklists.end()) && !it->second->workAvailable(core)) {
            it++;
        }
        
        // If can't find a priority with work right now, just keep the old val
        if(it!=unorderedWorklists.end()) {
            corePriorities[core] = it->first;
        }
    }
    return success;
}

void OBIM::putWork(Work work, uint64_t core) {
    uint64_t priority = work.priority/10000;
  
    if(unorderedWorklists.count(priority) == 0) {
        unorderedWorklists[priority] = new UnorderedWorklist(latency);
    }
    //std::cout << "putWork priority " << priority << std::endl;
    unorderedWorklists[priority]->putWork(work, core);
}

void OBIM::step() {
    timestep++;
  
    for(std::map<uint64_t, Worklist*>::iterator it = unorderedWorklists.begin(); it != unorderedWorklists.end(); it++) {
        it->second->step();
    }
}

bool OBIM::workAvailable(uint64_t core) {
    return unorderedWorklists[corePriorities[core]]->workAvailable(core);
}

bool OBIM::notEmpty() {
  
    for(std::map<uint64_t, Worklist*>::iterator it = unorderedWorklists.begin(); it != unorderedWorklists.end(); it++) {
        if(it->second->notEmpty()) {
            return true;
        }
    }
  
    return false;
}

uint64_t OBIM::size() {
  
    uint64_t totalSize = 0;
  
    for(std::map<uint64_t, Worklist*>::iterator it = unorderedWorklists.begin(); it != unorderedWorklists.end(); it++) {
        totalSize += it->second->size();
    }
    return totalSize;
}
