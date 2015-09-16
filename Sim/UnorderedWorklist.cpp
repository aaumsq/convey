
#include <vector>
#include <stdio.h>
#include "UnorderedWorklist.h"


UnorderedWorklist::UnorderedWorklist(unsigned latency) {
    worklist = new std::queue<Work>();
    futureWorklist = new std::priority_queue<Work, std::vector<Work>, CompareTime>();
    timestep = 0;
    this->latency = latency;
}

UnorderedWorklist::~UnorderedWorklist() {
    delete worklist;
    delete futureWorklist;
}

bool UnorderedWorklist::getWork(Work& work, uint64_t core) {
    if(worklist->empty()) {
        return false;
    }
    
    work = worklist->front();
    worklist->pop();
    return true;
}

void UnorderedWorklist::putWork(Work work, uint64_t core) {
    work.timestep = timestep + latency;
    futureWorklist->push(work);
}

void UnorderedWorklist::step() {
    timestep++;
    
    while(!futureWorklist->empty() && futureWorklist->top().timestep <= timestep) {
        worklist->push(futureWorklist->top());
        futureWorklist->pop();
    }
}

bool UnorderedWorklist::workAvailable(uint64_t core) {
    return !worklist->empty();
}

bool UnorderedWorklist::notEmpty() {
    return !(worklist->empty() && futureWorklist->empty());
}

uint64_t UnorderedWorklist::size() {
    return worklist->size() + futureWorklist->size();
}
