
#include <vector>
#include <stdio.h>
#include "UnorderedWorklist.h"


UnorderedWorklist::UnorderedWorklist() {
    worklist = new std::priority_queue<Work, std::vector<Work>, std::greater<Work> >();
    timestep = 0;
}

bool UnorderedWorklist::getWork(Work& work, uint64_t core) {
    if(worklist->empty()) {
        return false;
    }
    
    if(worklist->top().timestep <= timestep) {
        work = worklist->top();
        worklist->pop();
        return true;
    }
    return false;
}

void UnorderedWorklist::putWork(Work work, uint64_t core) {
    work.priority = 0;
    worklist->push(work);
}

void UnorderedWorklist::step() {
    timestep++;
}

bool UnorderedWorklist::notEmpty() {
    return !worklist->empty();
}

uint64_t UnorderedWorklist::size() {
    return worklist->size();
}
