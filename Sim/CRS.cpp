/* Row-indexed Sparse Matrix */
#include <iostream>
#include <utility>
#include "CRS.h"

using namespace std;

CRS::CRS() {
  row = new vector<deque<pair<unsigned, unsigned> >*>();
}

CRS::~CRS() {
  
}

void CRS::add(unsigned r, unsigned c, unsigned weight) {
  if((r+1) > row->size()) {
    row->resize(r+1);
    //cout << "Resizing to " << row->size() << endl;
  }
  if(row->at(r) == NULL) {
    row->at(r) = new deque<pair<unsigned, unsigned> >();
    //cout << "Adding new queue\n";
  }
  /*
  for(std::deque<pair<unsigned,unsigned> >::iterator it = row->at(r)->begin(); it!=row->at(r)->end(); it++) {
    if(it->first == c) {
      //cout << "WARNING: duplicate edge " << c << ", old weight = " << it->second << ", new weight = " << weight << endl;
      return;
    }
  }
  */
  row->at(r)->push_back(make_pair(c, weight));
  //cout << "Pushing " << c << " to row " << r << endl;
}

deque<pair<unsigned, unsigned> >* CRS::getRow(unsigned r) {
  return row->at(r);
}

void CRS::print() {
  cout << "Num rows: " << row->size() << endl;
  
  for(int i = 0; i < row->size(); i++) {
    if(row->at(i) != NULL) 
      cout << "row[" << i << "] size = " << row->at(i)->size() << endl;
  }
}
