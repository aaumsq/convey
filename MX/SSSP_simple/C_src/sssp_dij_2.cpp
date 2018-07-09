#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <cassert>
#include <time.h>
#include <queue>
#include <chrono>
#include <ctime>

//#include <boost/heap/fibonacci_heap.hpp>

#include "CRS.h"

using namespace std;
using namespace std::chrono;

CRS data;
bool* relaxed;
unsigned* totalDist;

class ComparePair {
public:
  bool operator()(pair<unsigned,unsigned> n1, pair<unsigned,unsigned> n2) {
    if(n1.second > n2.second)
      return true;
    else
      return false;
  }
};
priority_queue<pair<unsigned, unsigned>, vector<pair<unsigned, unsigned> >, ComparePair> work;

double abs(double val) {
  if(val < 0)
    return -1.0 * val;
  else
    return val;
}

unsigned min(unsigned a, unsigned b) {
  return (a < b) ? a : b;
}

int main(int argc, char** argv) {
  
  if((argc != 3) && (argc != 5)) {
    cout << "ERROR: incorrect input parameters!\n";
    cout << argv[0] << " <input file name> <source vertex>\n-- OR --\n";
    cout << argv[0] << " <input file name> <source vertex> -out <output file name>" << endl;
    exit(1);
  }
  
  ifstream in(argv[1]);
  unsigned source = atoi(argv[2]);
  bool genOutput = false;
  ofstream out(argv[4]);
  if(argc == 5) {
    genOutput = true;
  }
  
  if(!in.is_open()) {
    cout << "ERROR: Can't open file " << argv[1] << endl;
    exit(1);
  }
  cout << "Running on " << argv[1] << " with source vertex " << source << endl;
  //time_t t1, t2, t3;
  //t1 = time(NULL);
  time_point<system_clock> start, end;
  
  // Get vertices and edges
  unsigned numVertices, numEdges;
  in >> numVertices >> ws >> numEdges >> ws;
  
  totalDist = new unsigned[numVertices];
  relaxed = new bool[numVertices];
  for(int i = 0; i < numVertices; i++) {
    relaxed[i] = false;
    totalDist[i] = -1;
  }
  cout << "EdgeDist initialized to " << totalDist[0] << endl;
  totalDist[source] = 0;
  work.push(make_pair(source, 0));
  
  int src, dest, weight;
  data.row->resize(numVertices);
  while(in.good()) {
    in >> src >> ws >> dest >> ws >> weight >> ws;
    //cout << "---" << src << " " << dest << " " << weight << endl;
    data.add(src, dest, weight);
  }
  //data.print();
  //t2 = time(NULL);
  long numEdgesExecuted = 0;
  cout << "Number of rows: " << data.row->size() << endl;

  cout << "Running Dijkstra's Algorithm\n";
  start = system_clock::now();
  while(!work.empty()) {
    pair<unsigned, unsigned> top = work.top();
    work.pop();

    //cout << "Top: " << top.first << ", " << top.second << endl;
    
    int vertex = top.first;
    if(relaxed[vertex])
      continue;
    
    // Check for disconnected graphs!
    if(totalDist[vertex] == -1) {
      cout << "Disconnected graph detected, break!\n";
      break;
    }
    
    relaxed[vertex] = true;

    deque<pair<unsigned, unsigned> >* row = data.getRow(vertex);
    if(row != NULL) {
      for(deque<pair<unsigned,unsigned> >::iterator it = row->begin(); it!=row->end(); it++) {
        numEdgesExecuted += 1;
	if(!relaxed[it->first]) {
	  // update distance
	  int dist = totalDist[vertex]+it->second;
	  if(dist < totalDist[it->first]) {
	    totalDist[it->first] = dist;
	    //cout << "  Checking " << it->first << ", minDist = " << minDist << endl;
	    work.push(make_pair(it->first, dist));
	  }
	}
      }
    }
  }
  
  end = system_clock::now();
  duration<double> elapsed_seconds = end-start;
  
  //cout << "Setup time: " << (t2 - t1) << " seconds\n";
  cout << "Dijkstra SSSP time: " << elapsed_seconds.count() << " seconds\n";
  cout << "Total edges executed: " << numEdgesExecuted << "\n";
  
  if(genOutput) {
    for(int i = 0; i < numVertices; i++) {
      out <<  i << "," << totalDist[i] << endl;
    }
  }
}
