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

typedef map<string, long double> weightedplayervector;  // c(ham=3,lec=5) names=ham/lec-- weights 3,5 (3ham+5lec)^n
typedef map<string, long double> playerstrengths;  // c(ham=0.3,lec=0.2,lam=0.5) names=ham/lec/lam -- 
typedef map<weightedplayervector, long double> hyper3;  // value = power
