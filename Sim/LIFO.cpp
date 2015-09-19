
#include <vector>
#include <iostream>
#include "LIFO.h"


LIFO::LIFO(unsigned latency) {
    worklist = new std::stack<Work>();
    futureWorklist = new std::priority_queue<Work, std::vector<Work>, CompareTime>();
    timestep = 0;
    this->latency = latency;
    
    std::cout << "Initializing LIFO scheduler, " << latency << " latency\n";
}

LIFO::~LIFO() {
    delete worklist;
    delete futureWorklist;
}

bool LIFO::getWork(Work& work, uint64_t core) {
    if(worklist->empty()) {
        return false;
    }
    
    work = worklist->top();
    worklist->pop();
    return true;
}

void LIFO::putWork(Work work, uint64_t core) {
    work.timestep = timestep + latency;
    futureWorklist->push(work);
}

void LIFO::step() {
    timestep++;
    
    while(!futureWorklist->empty() && futureWorklist->top().timestep <= timestep) {
        worklist->push(futureWorklist->top());
        futureWorklist->pop();
    }
}

bool LIFO::workAvailable(uint64_t core) {
    return !worklist->empty();
}

bool LIFO::notEmpty() {
    return !(worklist->empty() && futureWorklist->empty());
}

uint64_t LIFO::size() {
    return worklist->size() + futureWorklist->size();
}
