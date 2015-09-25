
#include <cstdlib>
#include <iostream>
#include "OBIM_HW.h"


OBIM_HW::OBIM_HW(uint64_t numCores, uint32_t latency, uint32_t bucketSize, uint32_t idealNumBuckets) {
  
    this->numCores = numCores;
    this->latency = latency;
    this->bucketSize = bucketSize;
    this->timestep = 0;
    this->minBucketSize = 1;
    this->numBuckets = idealNumBuckets;
    this->maxBucketSize = 1048576;
    this->bucketThreshold = 1;
    
    corePriorities.resize(numCores);
    for(int i = 0; i < corePriorities.size(); i++) {
        corePriorities[i] = 0;
    }
    
    std::cout << "Initializing OBIM_HW scheduler, " << numCores << " cores, " << latency << " latency, " << bucketSize << " bucketSize\n";
}

bool OBIM_HW::getWork(Work& work, uint64_t core) {
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
        std::map<uint64_t, Worklist*>::iterator it = unorderedWorklists.begin();
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
        
        // Try again
        if(unorderedWorklists.count(priority) > 0) {
            found = true;
            success = unorderedWorklists[corePriorities[core]]->getWork(work, core);
            //std::cout << "Trying again with priority " << corePriorities[core] << ", success = " << success << "\n";
        }
    }
    return success;
}

void OBIM_HW::putWork(Work work, uint64_t core) {
    uint64_t priority = work.priority/bucketSize;
    
    if(unorderedWorklists.count(priority) == 0) {
        //unorderedWorklists[priority] = new Worklist(latency);
        //unorderedWorklists[priority]->putWork(work, core);
        
        OBIMHWPkt pkt = {timestep + latency, priority, work, core};
        futurePriorities.push(pkt);
        //std::cout << "Enqueueing priority " << priority << ", future latency " << timestep+latency << "\n";
        
    }
    else {
      unorderedWorklists[priority]->putWork(work, core);
    }
}

void OBIM_HW::step() {
    timestep++;
    
    // Adjust buckets
    if(timestep % 100 == 0) {
        //std::cout << "Checking buckets, currently there are " << unorderedWorklists.size() << ", target number " << numBuckets << ", threshold " << bucketThreshold << "\n";
        if(unorderedWorklists.size() < (numBuckets - bucketThreshold)) {
            // If too few buckets, increase number of buckets by decreasing interval
            if(bucketSize/2 < minBucketSize)
                bucketSize = minBucketSize;
            else
                bucketSize = bucketSize / 2;
            
            std::cout << "Decreasing bucket size to " << bucketSize << std::endl;
        }
        else if(unorderedWorklists.size() > (numBuckets + bucketThreshold)) {
            if(bucketSize*2 > maxBucketSize)
                bucketSize = maxBucketSize;
            else
                bucketSize = bucketSize * 2;
            
            std::cout << "Increasing bucket size to " << bucketSize << std::endl;
        }
    }
    
    // Step unordered worklists
    for(std::map<uint64_t, Worklist*>::iterator it = unorderedWorklists.begin(); it != unorderedWorklists.end(); it++) {
        it->second->step();
    }
    
    // Step priority map
    while(!futurePriorities.empty() && futurePriorities.top().timestep <= timestep) {
        OBIMHWPkt pkt = futurePriorities.top();
        if(unorderedWorklists.count(pkt.priority) == 0) {
            unorderedWorklists[pkt.priority] = new RandomWorklist(latency);
        }
        
        unorderedWorklists[pkt.priority]->putWork(pkt.work, pkt.core);
        futurePriorities.pop();
    }
}

bool OBIM_HW::workAvailable(uint64_t core) {
    return unorderedWorklists[corePriorities[core]]->workAvailable(core);
}

bool OBIM_HW::notEmpty() {
  
    for(std::map<uint64_t, Worklist*>::iterator it = unorderedWorklists.begin(); it != unorderedWorklists.end(); it++) {
        if(it->second->notEmpty()) {
            return true;
        }
    }
    
    if(!futurePriorities.empty()) {
        return true;
    }
    
    return false;
}

uint64_t OBIM_HW::size() {
  
    uint64_t totalSize = 0;
  
    for(std::map<uint64_t, Worklist*>::iterator it = unorderedWorklists.begin(); it != unorderedWorklists.end(); it++) {
        totalSize += it->second->size();
    }
    
    totalSize += futurePriorities.size();
    return totalSize;
}
