/* Row-indexed Sparse Matrix */
#ifndef _CRS_H_
#define _CRS_H_
#include <vector>
#include <deque>
#include <utility>

using namespace std;

class CRS {
  
 public:
  vector<deque<pair<unsigned, unsigned> >*> *row;
  CRS();
  ~CRS();
  
  void add(unsigned row, unsigned col, unsigned weight);
  deque<pair<unsigned, unsigned> >* getRow(unsigned row);
  void print();
};

#endif
