#ifndef __GRAPH__
#define __GRAPH__

#include <iostream>
#include <stdint.h>

struct Node {
    uint64_t edgePtr;
    uint64_t numEdges;
    uint64_t payload;
    uint64_t id;
    double   pagerank;
};

struct Edge {
    uint64_t weight;
    uint64_t dest;
};

class Graph {
    Node* nodes;
    Edge* edges;

public: 
    bool* nodeLocks;
    
    unsigned long long numNodes, numEdges;
    Graph();
    inline Node* getNode(uint64_t id) {
        return nodes + id;
    }
    inline Edge* getEdge(uint64_t id) {
        return edges + id;
    }
    void addNode(Node node);
    void clearLocks();
    bool loadEdgelistFile(const char* file);
};

#endif
