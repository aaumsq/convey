#include <iostream>
#include <cstdlib>

#include "RandomWorklist.h"


RandomWorklist::RandomWorklist(unsigned latency) {
    worklist = new std::vector<Work>();
    futureWorklist = new std::priority_queue<Work, std::vector<Work>, CompareTime>();
    timestep = 0;
    this->latency = latency;
    
    //std::cout << "RandomWorklist latency " << latency << std::endl;
}

bool RandomWorklist::getWork(Work& work, uint64_t core) {
    if(worklist->empty()) {
        return false;
    }
    
    uint32_t idx = rand() % worklist->size();
    
    work = worklist->at(idx);
    worklist->at(idx) = worklist->back();
    worklist->pop_back();
    return true;
}

void RandomWorklist::putWork(Work work, uint64_t core) {
    work.timestep = timestep + latency;
    futureWorklist->push(work);
}

void RandomWorklist::step() {
    timestep++;
    
    while(!futureWorklist->empty() && futureWorklist->top().timestep <= timestep) {
        worklist->push_back(futureWorklist->top());
        futureWorklist->pop();
    }
}

bool RandomWorklist::workAvailable(uint64_t core) {
    return !worklist->empty();
}

bool RandomWorklist::notEmpty() {
    return !(worklist->empty() && futureWorklist->empty());
}

uint64_t RandomWorklist::size() {
    return worklist->size() + futureWorklist->size();
}
