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
#include "LIFO.h"
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
    uint64_t conflictsPerCurIter = 0;
    uint64_t totalWork = 0;
    uint64_t totalWorkIssued = 0;
    uint64_t totalWorkGen = 0;
    uint64_t totalConflicts = 0;
    bool infCores = false;
    uint64_t maxCores = 128;
    uint64_t maxWork = 0;
    
    Graph* graph = new Graph();
    Graph* idealGraph = new Graph();
    //Worklist* worklist = new UnorderedWorklist(0);
    //Worklist* worklist = new LIFO(0);
    Worklist* worklist = new OrderedWorklist(0, 1);
    //Worklist* worklist = new LocalOrderedWorklist(maxCores, 64, 10);
    //Worklist* worklist = new OBIM(128, 10, 10000);
    
    std::cout << "Running on " << argv[1] << " with source vertex " << source << std::endl;
    time_t t1, t2, t3;
    t1 = time(NULL);
    
    graph->loadEdgelistFile(argv[1]);
    idealGraph->loadEdgelistFile(argv[1]);
    
    std::cout << "Done loading\n";

    Work initWork = {source, 0, 0};
    worklist->putWork(initWork, 0);
    graph->getNode(source)->payload = 0;
    t2 = time(NULL);

    Work work;
    std::vector<uint64_t>* workPerIter = new std::vector<uint64_t>();
    std::vector<uint64_t>* conflictsPerIter = new std::vector<uint64_t>();
    
    std::priority_queue<Work, std::vector<Work>, ComparePriority> idealWL;
    idealWL.push(initWork);
    idealGraph->getNode(source)->payload = 0;
    
    uint64_t idealIters = 0;
    uint64_t idealWork = 0;
    while(!idealWL.empty()) {
        idealIters++;
        work = idealWL.top();
        idealWL.pop();
        Node* curNode = idealGraph->getNode(work.graphId);
        for(int i = 0; i < curNode->numEdges; i++) {
            Edge* edge = idealGraph->getEdge(curNode->edgePtr + i);
            Node* destNode = idealGraph->getNode(edge->dest);
            if(curNode->payload + edge->weight < destNode->payload) {
                destNode->payload = curNode->payload + edge->weight;
                Work newWork = {edge->dest, destNode->payload, iters+1};
                idealWL.push(newWork);
                idealWork++;
            }
        }
    }
    std::cout << "IdealIters: " << idealIters << ", IdealWork: " << idealWork << "\n";
    
    while(worklist->notEmpty()) {
        workPerCurIter = 0;
        workGenPerCurIter = 0;
        conflictsPerCurIter = 0;
        
        graph->clearLocks();
        
        if(infCores)
            maxCores = worklist->size();
        
        for(int x = 0; x < maxCores; x++) {
            bool hasWork = true; 
            bool success = false;
            
            while(!success) {
                hasWork = worklist->getWork(work, x);
                if(!hasWork)
                    break; 
                
                totalWorkIssued++;
                
                Node* curNode = graph->getNode(work.graphId);
                Node* curIdealNode = idealGraph->getNode(work.graphId);
                
                if(curNode->payload != curIdealNode->payload)
                    continue;
                
                // Check locks
                bool abort = false;
                
                if(graph->nodeLocks[work.graphId]) {
                    abort = true;
                }
                else {
                    //curNode = graph->getNode(work.graphId);
                    
                    for(int i = 0; i < curNode->numEdges; i++) {
                        Edge* edge = graph->getEdge(curNode->edgePtr + i);
                        Node* destNode = graph->getNode(edge->dest);
                        if(graph->nodeLocks[edge->dest]) {
                            abort = true;
                        }
                    }
                }
                
                // Someone else is using it, abort and retry later
                if(abort) {
                    conflictsPerCurIter++;
                    totalConflicts++;
                    worklist->putWork(work, x);
                    success = false;
                }
                else {
                    // If successful, grab locks and continue
                    assert(!abort);
                    graph->nodeLocks[work.graphId] = true;
                    for(int i = 0; i < curNode->numEdges; i++) {
                        Edge* edge = graph->getEdge(curNode->edgePtr + i);
                        Node* destNode = graph->getNode(edge->dest);
                        Node* oracleDestNode = idealGraph->getNode(edge->dest);
                        
                        graph->nodeLocks[edge->dest] = true;
                        
                        if(curNode->payload + edge->weight < destNode->payload) {
                        //if(curNode->payload + edge->weight == oracleDestNode->payload) {
                            destNode->payload = curNode->payload + edge->weight;
                            
                            success = true;
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
        }
        
        if(iters % 1000 == 0) {
            std::cout << "Iter " << iters << ": completed " << workPerCurIter << " work items, max " << maxCores << ", " << conflictsPerCurIter << " conflicts, worklist size: " << worklist->size() << ", gen work: " << workGenPerCurIter << ", total gen work: " << totalWorkGen << "\n";
        }
        workPerIter->push_back(workPerCurIter);
        conflictsPerIter->push_back(conflictsPerCurIter);
        worklist->step();
        iters++;
        
        if(workPerCurIter > maxWork)
            maxWork = workPerCurIter;
    }
    
    t3 = time(NULL);
    
    std::cout << "Setup time: " << (t2 - t1) << " seconds\n";
    std::cout << "SSSP time: " << (t3 - t2) << " seconds\n";
    
    if(infCores)
        maxCores = maxWork;
    
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
    
    std::ofstream out2("./stats.csv");
    out2 << "Iteration, Work Completed, Conflicts\n";
    for(int i = 0; i < workPerIter->size(); i++) {
        out2 << i << ", " << workPerIter->at(i) << ", " << conflictsPerIter->at(i) << "\n";
    }
    out2.close();
}
