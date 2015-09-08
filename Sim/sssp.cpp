#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <cassert>
#include <time.h>
#include <queue>
#include <vector>

#include "Worklist.h"
#include "UnorderedWorklist.h"
#include "OrderedWorklist.h"
#include "LocalOrderedWorklist.h"
#include "OBIM.h"
#include "Graph.h"

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
    uint64_t totalWork = 0;
    uint64_t totalWorkGen = 0;
    bool infCores = false;
    uint64_t maxCores = 128;
    uint64_t maxWork = 0;
    
    Graph* graph = new Graph();
    //Worklist* worklist = new UnorderedWorklist(100);
    //Worklist* worklist = new OrderedWorklist();
    //Worklist* worklist = new LocalOrderedWorklist(maxCores, 64, 10);
    Worklist* worklist = new OBIM(128, 10);
    
    std::cout << "Running on " << argv[1] << " with source vertex " << source << std::endl;
    time_t t1, t2, t3;
    t1 = time(NULL);
    
    graph->loadEdgelistFile(argv[1]);
    
    std::cout << "Done loading\n";

    Work initWork = {source, 0, 0};
    worklist->putWork(initWork, 0);
    graph->getNode(source)->payload = 0;
    t2 = time(NULL);

    Work work;
    std::vector<uint64_t>* workPerIter = new std::vector<uint64_t>();

    while(worklist->notEmpty()) {
        workPerCurIter = 0;
        workGenPerCurIter = 0;
        
        for(int x = 0; x < maxCores; x++) {
            bool hasWork = worklist->getWork(work, x);
            if(hasWork) {
                Node* curNode = graph->getNode(work.graphId);
                
                for(int i = 0; i < curNode->numEdges; i++) {
                    Edge* edge = graph->getEdge(curNode->edgePtr + i);
                    Node* destNode = graph->getNode(edge->dest);
                    if(curNode->payload + edge->weight < destNode->payload) {
                        destNode->payload = curNode->payload + edge->weight;
                        
                        Work newWork = {edge->dest, destNode->payload, iters+1};
                        worklist->putWork(newWork, x);
                        workGenPerCurIter++;
                        totalWorkGen++;
                    }
                }
                workPerCurIter++;
                totalWork++;
            }
        }
        
        if(iters % 100 == 0) {
            std::cout << "Iter " << iters << ": completed " << workPerCurIter << " work items, max " << maxCores << ", worklist size: " << worklist->size() << ", gen work: " << workGenPerCurIter << ", total gen work: " << totalWorkGen << "\n";
        }
        workPerIter->push_back(workPerCurIter);
        worklist->step();
        iters++;
        
        if(workPerCurIter > maxWork)
            maxWork = workPerCurIter;
    }
    
    t3 = time(NULL);
    
    std::cout << "Setup time: " << (t2 - t1) << " seconds\n";
    std::cout << "Dijkstra SSSP time: " << (t3 - t2) << " seconds\n";
    
    std::cout << "Iters: " << iters << ", totalWork: " << totalWork << ", max cores active: " << maxWork 
              << ", utilization: " << double(totalWork)/double(iters*maxCores) << std::endl;

    if(genOutput) {
        for(int i = 0; i < graph->numNodes; i++) {
            out << graph->getNode(i)->id << "," << graph->getNode(i)->payload << "\n";
        }
        out.close();
    }
    
    std::ofstream out2("./stats.csv");
    out2 << "Iteration, Work Completed\n";
    for(int i = 0; i < workPerIter->size(); i++) {
        out2 << i << ", " << workPerIter->at(i) << "\n";
    }
    out2.close();
}
