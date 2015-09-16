
#include <cstdlib>
#include <iostream>
#include "OBIM.h"


OBIM::OBIM(uint64_t numCores, uint32_t latency, uint32_t bucketSize) {
  
    this->numCores = numCores;
    this->latency = latency;
    this->bucketSize = bucketSize;
    this->timestep = 0;
    
    corePriorities.resize(numCores);
    for(int i = 0; i < corePriorities.size(); i++) {
        corePriorities[i] = 0;
    }
    
    std::cout << "Initializing OBIM scheduler, " << numCores << " cores, " << latency << " latency, " << bucketSize << " bucketSize\n";
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
            delete unorderedWorklists[priority];
            unorderedWorklists.erase(priority);
        }
        
        //corePriorities[core] = unorderedWorklists.begin()->first;
        
        // Find highest priority that has available work right now
        std::map<uint64_t, UnorderedWorklist*>::iterator it = unorderedWorklists.begin();
        unsigned skipped = 0;
        while((it!=unorderedWorklists.end()) && !it->second->workAvailable(core)) {
            it++;
            skipped++;
            //if(it != unorderedWorklists.end())
            //    std::cout << "Skipping " << skipped << ", no work available, size: " << it->second->size() << "\n";
        }
        
        // If can't find a priority with work right now, just keep the old val
        if(it!=unorderedWorklists.end()) {
            //std::cout << "Changing priority " << corePriorities[core] << "->" << it->first << ", skipped " << skipped << "\n";
            corePriorities[core] = it->first;
        }
        
    }
    return success;
}

void OBIM::putWork(Work work, uint64_t core) {
    uint64_t priority = work.priority/bucketSize;
  
    if(unorderedWorklists.count(priority) == 0) {
        //unorderedWorklists[priority] = new UnorderedWorklist(latency);
        //unorderedWorklists[priority]->putWork(work, core);
        
        OBIMPkt pkt = {timestep + latency, priority, work, core};
        futurePriorities.push(pkt);
        //std::cout << "Enqueueing priority " << priority << ", future latency " << timestep+latency << "\n";
        
    }
    else {
      unorderedWorklists[priority]->putWork(work, core);
    }
}

void OBIM::step() {
    timestep++;
    
    // Step unordered worklists
    for(std::map<uint64_t, UnorderedWorklist*>::iterator it = unorderedWorklists.begin(); it != unorderedWorklists.end(); it++) {
        it->second->step();
    }
    
    // Step priority map
    while(!futurePriorities.empty() && futurePriorities.top().timestep <= timestep) {
        OBIMPkt pkt = futurePriorities.top();
        if(unorderedWorklists.count(pkt.priority) == 0) {
            unorderedWorklists[pkt.priority] = new UnorderedWorklist(latency);
        }
        
        unorderedWorklists[pkt.priority]->putWork(pkt.work, pkt.core);
        futurePriorities.pop();
    }
}

bool OBIM::workAvailable(uint64_t core) {
    return unorderedWorklists[corePriorities[core]]->workAvailable(core);
}

bool OBIM::notEmpty() {
  
    for(std::map<uint64_t, UnorderedWorklist*>::iterator it = unorderedWorklists.begin(); it != unorderedWorklists.end(); it++) {
        if(it->second->notEmpty()) {
            return true;
        }
    }
    
    if(!futurePriorities.empty()) {
        return true;
    }
    
    return false;
}

uint64_t OBIM::size() {
  
    uint64_t totalSize = 0;
  
    for(std::map<uint64_t, UnorderedWorklist*>::iterator it = unorderedWorklists.begin(); it != unorderedWorklists.end(); it++) {
        totalSize += it->second->size();
    }
    
    totalSize += futurePriorities.size();
    return totalSize;
}
