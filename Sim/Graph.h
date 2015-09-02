#ifndef __GRAPH__
#define __GRAPH__

#include <stdint.h>

struct Node {
    uint64_t edgePtr;
    uint64_t numEdges;
    uint64_t payload;
    uint64_t id;
};

struct Edge {
    uint64_t weight;
    uint64_t dest;
};

class Graph {
    Node* nodes;
    Edge* edges;
    
public: 
    unsigned long long numNodes, numEdges;
    Graph();
    Node* getNode(uint64_t id);
    Edge* getEdge(uint64_t id);
    void addNode(Node node);
    bool loadEdgelistFile(const char* file);
};

#endif
