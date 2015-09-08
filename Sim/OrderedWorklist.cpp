
#include "OrderedWorklist.h"


OrderedWorklist::OrderedWorklist() {
    worklist = new std::priority_queue<Work, std::vector<Work>, ComparePriority>();
    futureWorklist = new std::priority_queue<Work, std::vector<Work>, CompareTime>();
    timestep = 0;
    curPriority = 0;
}

bool OrderedWorklist::getWork(Work& work, uint64_t core) {
    if(worklist->empty()) {
        return false;
    }
    
    //if(worklist->top().priority == curPriority) {
    if(1) {
        work = worklist->top();
        worklist->pop();
        return true;
    }
    
    return false;
}

void OrderedWorklist::putWork(Work work, uint64_t core) {
    work.priority = work.priority/1024;
    futureWorklist->push(work);
}

void OrderedWorklist::step() {
    timestep++;
    
    while(!futureWorklist->empty() && futureWorklist->top().timestep <= timestep) {
        worklist->push(futureWorklist->top());
        futureWorklist->pop();
    }
    
    curPriority = worklist->top().priority;
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
