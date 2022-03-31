// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include <set>
#include <map>
#include <iostream>
#include <Rcpp.h>
#include <iterator>
#include <vector>
#include <assert.h>

using namespace std;
using namespace Rcpp;

typedef set<string> bracket;
typedef map<bracket, long double> hyper2;
typedef map<string, long double> psub;   // prob substitute object

// again it might be nice to use unsigned_map above, but this would
// need a hash function and this would require further work.

typedef map<string, long double> namedvector;  // c(ham=3,lec=1,lam=5) names=ham/lec/lam -- weights 3,1,5
typedef map<namedvector, long double> hyper3;  // value = power
