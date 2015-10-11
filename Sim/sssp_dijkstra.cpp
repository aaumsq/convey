#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <cassert>
#include <time.h>
#include <queue>
#include <vector>

#include <cpucounters.h>

#include "Graph.h"
#include "Worklist.h"

int main(int argc, char** argv) {
    
    if((argc != 3) && (argc != 5)) {
        std::cout << "ERROR: incorrect input parameters!\n";
        std::cout << argv[0] << " <input file name> <source vertex>\n-- OR --\n";
        std::cout << argv[0] << " <input file name> <source vertex> -out <output file name>" << std::endl;
        exit(1);
    }
    
    unsigned source = atoi(argv[2]);
    bool genOutput = false;
    std::ofstream out(argv[4]);
    if(argc == 5) {
        genOutput = true;
    }
    uint64_t iters = 0;
    uint64_t workPerCurIter = 0;
    uint64_t workGenPerCurIter = 0;
    uint64_t conflictsPerCurIter = 0;
    uint64_t totalWork = 0;
    uint64_t totalWorkIssued = 0;
    uint64_t totalWorkGen = 0;
    uint64_t totalConflicts = 0;
    bool infCores = false;
    uint64_t maxCores = 128;
    uint64_t maxWork = 0;
    
    Graph* graph = new Graph();
    std::priority_queue<Work, std::vector<Work>, ComparePriority> wl;
    
    std::cout << "Running on " << argv[1] << " with source vertex " << source << std::endl;
    time_t t1, t2, t3;
    t1 = time(NULL);
    
    graph->loadEdgelistFile(argv[1]);
    
    std::cout << "Done loading\n";

    Work initWork = {source, 0, 0};
    wl.push(initWork);
    graph->getNode(source)->payload = 0;
    
    t2 = time(NULL);
    
    PCM * m = PCM::getInstance();
    //m->resetPMU();
    
    // program counters, and on a failure just exit

    if (m->program() != PCM::Success) {
        m->cleanup();
        return -1;
    }
    
    SystemCounterState before_sstate = getSystemCounterState();

    Work work;
    
    uint64_t idealIters = 0;
    uint64_t idealWork = 0;
    while(!wl.empty()) {
        //idealIters++;
        work = wl.top();
        wl.pop();
        Node* curNode = graph->getNode(work.graphId);
        for(int i = 0; i < curNode->numEdges; i++) {
            Edge* edge = graph->getEdge(curNode->edgePtr + i);
            Node* destNode = graph->getNode(edge->dest);
            if(curNode->payload + edge->weight < destNode->payload) {
                destNode->payload = curNode->payload + edge->weight;
                Work newWork = {edge->dest, destNode->payload, iters+1};
                wl.push(newWork);
                //idealWork++;
            }
        }
    }
    std::cout << "IdealIters: " << idealIters << ", IdealWork: " << idealWork << "\n";
    
    
    SystemCounterState after_sstate = getSystemCounterState();
    
    std::cout << "Instructions per clock: " << getIPC(before_sstate,after_sstate)
              << "\nL3 cache hit ratio: " << getL3CacheHitRatio(before_sstate,after_sstate)
              << "\nBytes read: " << getBytesReadFromMC(before_sstate,after_sstate)
              << "\n";
    
    t3 = time(NULL);
    m->cleanup();
    
    std::cout << "Setup time: " << (t2 - t1) << " seconds\n";
    std::cout << "SSSP time: " << (t3 - t2) << " seconds\n";
    
    
    std::cout << "Iters: " << iters << ", totalWork: " << totalWork << ", max cores active: " << maxWork 
              << ", utilization: " << double(totalWorkIssued)/double(iters*maxCores) 
              << ", executed: " << double(totalWork)/double(iters*maxCores) 
              << ", conflict percent: " << double(totalConflicts)/double(iters*maxCores) << std::endl;
    
    if(genOutput) {
        for(int i = 0; i < graph->numNodes; i++) {
            out << graph->getNode(i)->id << "," << graph->getNode(i)->payload << "\n";
        }
        out.close();
    }
    
}
