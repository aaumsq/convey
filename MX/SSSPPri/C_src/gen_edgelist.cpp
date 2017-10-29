#include <iostream>
#include <fstream>
#include <set>
#include <stdio.h>
#include <stdlib.h>

using namespace std;

int main() {
  const int N = 16384;
  const int max_e_per_device = 131072;

  int* h_indices = new int[N+1];
  int* h_neighbors = new int[max_e_per_device];
  int* h_weights = new int[max_e_per_device];

  int count = 0;
  set<int> neighbor_record;
  for (int j = 0; j < N; j++) {
    h_indices[j] = count;
    if (count < max_e_per_device) {
      bool first = true;
      while (first || (rand() % 5) > 0) {  // degree = 3
        int neighbor = rand() % N;
        if (neighbor == j || (neighbor_record.find(neighbor) != neighbor_record.end())) continue;
        neighbor_record.insert(neighbor);
        h_neighbors[count] = neighbor;
        h_weights[count] = rand() % 100;
        count++;
        first = false;
        if (count == max_e_per_device) {  // full
          break;
        }
      }
      neighbor_record.clear();
    }
  }
  h_indices[N] = count;

  ofstream out("./test2.edgelist");
  out << N << " " << count << endl;

  for (int i = 0; i < N; i++) {
    for (int j = h_indices[i]; j < h_indices[i+1]; j++) {
      out << i << " " << h_neighbors[j] << " " << h_weights[j] << endl;
    }
  }

return 0;
  
}

