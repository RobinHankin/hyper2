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

typedef map<string, long double> namedvector;  // c(ham=3,lec=1,lam=5) names=ham/lec/lam -- weights 3,1,5
typedef map<namedvector, long double> hyper3;  // value = power


// again it might be nice to use unsigned_map above, but this would
// need a hash function and this would require further work.

hyper3 prepareL3(const List &L, const List &W, const NumericVector &d){
    hyper3 H;   
    namedvector nv;
    unsigned int i,j;
    const unsigned n=L.size();
    
    for(i=0; i<n ; i++){
        if(d[i] != 0){
            const SEXP jj = L[i];  // jj is temporary
            const SEXP kk = W[i];  // jj is temporary
            const Rcpp::CharacterVector names(jj);
            const Rcpp::CharacterVector weights(kk);
            for(j=0 ; j<(unsigned int) names.size(); j++){
                nv[(string) names[j]] = weights[j];
            }
            H[nv] += d[i];  // the meat
        } // else do nothing, a zero power 
    } // i loop closes
    return(H);
}

psub3 preparepmap3(const NumericVector &probs, const CharacterVector &pnames){
    psub3 out;

    for(int i=0; i<probs.length() ; i++){
        out[(string) pnames[i]] = probs[i];
    }
    return(out);
}

List makebrackets3(const hyper3 H){// Returns a list of players, eg list('a',c('a','b'),c('a','xt'))
    List out;
    hyper3::const_iterator ih;
    
    for(ih=H.begin(); ih != H.end(); ++ih){
        namedvector nv = ih->first;
        CharacterVector t;
        for(namedvector::iterator in=nv.begin() ; in != nv.end() ; in++){
            t[i] = in->first;
        }
        out.push_back(ip->first);
    }
    return(out);
}

List makeweights3(const hyper3 H){// Returns a list of players, eg list(1,1:2,2.2)
    List out;
    hyper3::const_iterator ih;
    
     for(ih=H.begin(); ih != H.end(); ++ih){
        namedvector nv = ih->first;
        CharacterVector t;
        for(namedvector::iterator in=nv.begin() ; in != nv.end() ; in++){
            t[i] = in->second;  // cf makebrackets3() [otherwise identical]
        }
        out.push_back(ip->first);
     }
     return(out);
 }
 
namedvector makenamedvector3(const CharacterVector players, const NumericVector weights){
    assert(players.size() == weights.size());
    namedvector out;
    for(int i=0 ; i<players.size() ; i++){
        out[players[i]] = weights[i];
    }
    return(out);
}

NumericVector makepowers3(const hyper3 H){
    NumericVector out(H.size());   // data
    unsigned int i=0;
    hyper3::const_iterator it;
    
    for(it=H.begin(); it != H.end(); ++it){
        out(i++) = it->second;
    }
    return(out);
}

List retval3(const hyper3 &H){  // used to return a list to R
    return List::create(
                        Named("brackets") =  makebrackets3(H),
                        Named("weights")  =  makeweights3(H),
                        Named("powers")   =  makepowers3(H)
                        );
}


// [[Rcpp::export]]
List identityL3(const List &L, const List &W, const NumericVector &p){
    const hyper3 out = prepareL3(L,W,p);
    return retval3(out);
}
 
//[[Rcpp::export]]
List addL3(
           const List L1, const List W1, const NumericVector p1,
           const List L2, const List W2, const NumericVector p2
          ){
    hyper3 h1 = prepareL3(L1,W1,p1);
    hyper3 h2 = prepareL3(L2,W2,p2);
    hyper3::const_iterator it;
    if(L1.size() > L2.size()){ // L1 is bigger, so iterate through L2
        for (it=h2.begin(); it != h2.end(); ++it){
            const namedvector n = it->first;
            h1[n] += h2[n];  // the meat
        }
        return(retval(h1));
    } else {  // L2 is bigger
        for (it=h1.begin(); it != h1.end(); ++it){
            const namedvector n = it->first;
            h2[n] += h1[n];  
        }
        return(retval(h2));
    }
}

//[[Rcpp::export]]
bool equality3(  // modelled on spray_equality()
               const List L1, const List W1, const NumericVector p1,
               const List L2, const List W2, const NumericVector p2
           ){
    hyper3 h1,h2;
    hyper3::const_iterator it;
    
    if(L1.size() != L2.size()){
        return false;
    }

    h1 = prepareL3(L1,W1,p1);
    h2 = prepareL3(L2,W2,p2);

    for (it=h1.begin(); it != h1.end(); ++it){
            const namedvector n = it->first;
            if(h1[n] != h2[n]){
                return false;
            } else {
                h2.erase(n);
            }
        }

    if(h2.empty()){
        return true;
    } else {
        return false;
    }
}

//[[Rcpp::export]]
List accessor(
              const List L,
              const List W,
              const NumericVector powers,
              const List Lwanted,
              const List Wwanted
              ){

    const hyper3 h=prepareL3(L,W,powers);
    hyper3 out;
    namedvector n;
    const unsigned int n=Lwanted.size();
    unsigned int i,j;

    for(i=0; i<n ; i++){
            n.clear();
            const SEXP jj = Lwanted[i];
            const SEXP kk = Wwanted[i];
            const Rcpp::CharacterVector players(jj);
            const Rcpp::CharacterVector weights(kk);

            for(j=0 ; j<(unsigned int) names.size(); j++){
                b.insert((string) names[j]);
            }
            
            if(h.count(b)>0){
                out[b] = h.at(b);
            }
    }
    return retval3(out);
}
    
//[[Rcpp::export]]
List overwrite3(  // H1[] <- H2
                const List L1, const W1, const NumericVector powers1,
                const List L2, const W2, const NumericVector powers2
              ){

          hyper3 h1=prepareL3(L1,W1,powers1);
    const hyper3 h2=prepareL3(L2,W2,powers2);
    hyper3::const_iterator it;

    for(it=h2.begin(); it != h2.end(); ++it){
        const namedvector nv = it->first;
        h1[nv] = h2.nv(b);
    }
    return retval3(h1);
}

//[[Rcpp::export]]
List assigner3(  // H[L] <- v
               const List L, const List W, const NumericVector p,
               const List L2, const List W2,
               const NumericVector value
              ){
    hyper3 h=prepareL3(L,W,p);
    namedvector n;
    hyper3::const_iterator it;
    const unsigned int n=L2.size();
    unsigned int i,j;

    for(i=0 ; i<n ; i++){
        n.clear();
        const SEXP jj = L2[i];  // jj is temporary
        const SEXP kk = W2[i];  // kk is temporary

        const Rcpp::CharacterVector iv(jj);
        const Rcpp::NumericVector iw(kk);
        for(j=0 ; j<(unsigned int) iv.size(); j++){
            n[(string) iv[j]] = iw[j];
        } 
        h[n] = value[i]; // RHS might be zero in which case this entry is deleted from h
    }
    return retval3(h);
}
                     
//[[Rcpp::export]]
double evaluate3(  // returns log-likelihood
                const List &L,
                const List &W,  // weights
                const NumericVector &powers,
                const NumericVector &probs,
                const CharacterVector &pnames
                  ){

    const hyper3 h = prepareL3(L,W,powers);
    psub3 psp = preparepmap3(probs,pnames);
    psub3 psw = preparepmap3(weights,pnames);
    
    hyper3::const_iterator it;
    double out=0;
    double bracket_total=0;
    namedvector::const_iterator in;

    for (it=h.begin(); it != h.end(); ++it){
        const namedvector n = it->first;
        bracket_total = 0;
        for (in=n.begin(); in != n.end(); ++in){
            bracket_total += psp[*ib] * psw[*ib]; // the meat
        }
        out += log(bracket_total) * (it->second); // also the meat
    }
    return out;
}

// following functions have a natural interpretation in terms of
// hyper3 objects but not coded up for hyper3 (function headers
// included below for reference)

/*
  double differentiate_single_independent( // d(log-likelihod)/dp
  double differentiate_single_alln( // d(log-likelihod)/dp
  double second_derivative( // evaluate terms of the lower-right block of the bordered Hessian

\\[[Rcpp::export]]
List hessian_lowlevel

\\[[Rcpp::export]]
List differentiate

\\[[Rcpp::export]]
List differentiate_n

*/
