
#include <cstdlib>
#include <iostream>
#include "OBIM_HW2.h"


OBIM_HW2::OBIM_HW2(uint64_t numCores, uint32_t latency, uint32_t bucketInterval, uint32_t idealBucketSize) {
  
    this->numCores = numCores;
    this->latency = latency;
    this->timestep = 0;
    this->minBucketInterval = 8*1024;
    this->bucketInterval = bucketInterval;
    this->maxBucketInterval = 128*1024; //1048576;
    this->bucketSize = idealBucketSize;
    this->bucketThreshold = idealBucketSize/4;
    
    corePriorities.resize(numCores);
    for(int i = 0; i < corePriorities.size(); i++) {
        corePriorities[i] = 0;
    }
    
    goodEpochs = 0;
    badEpochs = 0;
    
    coreIssued.resize(numCores);
    for(int i = 0; i < coreIssued.size(); i++) {
        coreIssued[i] = 0;
    }

    std::cout << "Initializing OBIM_HW2 scheduler, " << numCores << " cores, " << latency << " latency, " << bucketSize << " bucketSize\n";
}

bool OBIM_HW2::getWork(Work& work, uint64_t core) {
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
    
    if(success)
        coreIssued[core]++;
    
    return success;
}

void OBIM_HW2::putWork(Work work, uint64_t core) {
    uint64_t priority = work.priority/bucketInterval;
    
    if(unorderedWorklists.count(priority) == 0) {
        //unorderedWorklists[priority] = new Worklist(latency);
        //unorderedWorklists[priority]->putWork(work, core);
        
        OBIMHW2Pkt pkt = {timestep + latency, priority, work, core};
        futurePriorities.push(pkt);
        //std::cout << "Enqueueing priority " << priority << ", future latency " << timestep+latency << "\n";
        
    }
    else {
      unorderedWorklists[priority]->putWork(work, core);
    }
}

void OBIM_HW2::step() {
    timestep++;
    
    // Adjust buckets
    uint64_t epochSize = 5000;
    if(timestep % epochSize == 0) {
        //std::cout << "Checking buckets, currently there are " << unorderedWorklists.size() << ", target number " << numBuckets << ", threshold " << bucketThreshold << "\n";

        uint64_t totalBubbles = 0;
        for(int i = 0; i < numCores; i++) {
            //std::cout << "core[" << i << "] issued = " << coreIssued[i] << std::endl;
            totalBubbles += epochSize - coreIssued[i];
        }
        double bubblePercent = 100.0*double(totalBubbles)/(double(numCores)*double(epochSize));
        std::cout << "total bubbles: " << totalBubbles << ", " << bubblePercent << "%, good epochs: " << goodEpochs << ", bad epochs: " << badEpochs << std::endl;
        
        
        // Calculate average WL size
        uint64_t totalSize = 0;
        for(std::map<uint64_t, Worklist*>::iterator it = unorderedWorklists.begin(); it != unorderedWorklists.end(); it++) {
            totalSize += it->second->size();
        }
        double avgSize = double(totalSize) / double(unorderedWorklists.size());
        

        
        //if(avgSize > (bucketSize + bucketThreshold)) {
        if(totalBubbles < 0.0005*numCores*epochSize) {
            badEpochs = 0;
            
            if(goodEpochs > 3) {
                goodEpochs = 0;
                // If too few buckets, increase number of buckets by decreasing interval
                if(bucketInterval/2 < minBucketInterval)
                    bucketInterval = minBucketInterval;
                else
                    bucketInterval = bucketInterval / 2;
                
                std::cout << "Buckets have avg size " << avgSize << ", decreasing bucket interval to " << bucketInterval << std::endl;
            }
            else {
                goodEpochs++;
            }
        }
        //else if(unorderedWorklists.begin()->second->size() < (bucketSize - bucketThreshold)) {
        //else if(avgSize < (bucketSize - bucketThreshold)) {
        else if(totalBubbles >= 0.0005*numCores*epochSize) {
            goodEpochs = 0;
            
            if(badEpochs > 0) {
                badEpochs = 0;
                if(bucketInterval*2 > maxBucketInterval)
                    bucketInterval = maxBucketInterval;
                else
                    bucketInterval = bucketInterval * 2;
                
                std::cout << "Buckets have avg size " << avgSize << ", increasing bucket interval to " << bucketInterval << std::endl;
            }
            else {
                badEpochs++;
            }
        }
        
        for(int i = 0; i < numCores; i++)
            coreIssued[i] = 0;
    }
    
    
    // Step unordered worklists
    for(std::map<uint64_t, Worklist*>::iterator it = unorderedWorklists.begin(); it != unorderedWorklists.end(); it++) {
        it->second->step();
    }
    
    // Step priority map
    while(!futurePriorities.empty() && futurePriorities.top().timestep <= timestep) {
        OBIMHW2Pkt pkt = futurePriorities.top();
        if(unorderedWorklists.count(pkt.priority) == 0) {
            unorderedWorklists[pkt.priority] = new RandomWorklist(latency);
        }
        
        unorderedWorklists[pkt.priority]->putWork(pkt.work, pkt.core);
        futurePriorities.pop();
    }
}

bool OBIM_HW2::workAvailable(uint64_t core) {
    return unorderedWorklists[corePriorities[core]]->workAvailable(core);
}

bool OBIM_HW2::notEmpty() {
  
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

uint64_t OBIM_HW2::size() {
  
    uint64_t totalSize = 0;
  
    for(std::map<uint64_t, Worklist*>::iterator it = unorderedWorklists.begin(); it != unorderedWorklists.end(); it++) {
        totalSize += it->second->size();
    }
    
    totalSize += futurePriorities.size();
    return totalSize;
}
