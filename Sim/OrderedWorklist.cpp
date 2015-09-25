#include <iostream>

#include "OrderedWorklist.h"


OrderedWorklist::OrderedWorklist(unsigned latency, uint64_t bucketSize) {
    worklist = new std::priority_queue<Work, std::vector<Work>, ComparePriority>();
    futureWorklist = new std::priority_queue<Work, std::vector<Work>, CompareTime>();
    timestep = 0;
    this->latency = latency;
    this->bucketSize = bucketSize;
    
    std::cout << "OrderedWorklist latency " << latency << ", bucketSize " << bucketSize << std::endl;
}

bool OrderedWorklist::getWork(Work& work, uint64_t core) {
    if(worklist->empty()) {
        return false;
    }
    
    work = worklist->top();
    worklist->pop();
    return true;
}

void OrderedWorklist::putWork(Work work, uint64_t core) {
    work.priority = work.priority/bucketSize;
    work.timestep = timestep + latency;
    futureWorklist->push(work);
}

void OrderedWorklist::step() {
    timestep++;
    
    while(!futureWorklist->empty() && futureWorklist->top().timestep <= timestep) {
        worklist->push(futureWorklist->top());
        futureWorklist->pop();
    }
}

bool OrderedWorklist::workAvailable(uint64_t core) {
    return !worklist->empty();
}

bool OrderedWorklist::notEmpty() {
    return !(worklist->empty() && futureWorklist->empty());
}

uint64_t OrderedWorklist::size() {
    return worklist->size() + futureWorklist->size();
}
