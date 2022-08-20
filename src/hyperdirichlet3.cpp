// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include "hyperdirichlet2.h"

weightedplayervector makeweightedplayervector3(const CharacterVector players, const NumericVector weights){
    assert(players.size() == weights.size());
    weightedplayervector out;
    for(size_t i=0 ; i < (size_t) players.size() ; i++){
        if(weights[i] != 0){
            out[(string) players[i]] += weights[i];
        }
    }
    return(out);
}

hyper3 prepareL3(const List &L, const List &W, const NumericVector &d){
    hyper3 H;   
    weightedplayervector nv;
    const unsigned n=L.size();
    
    for(size_t i=0; i<n ; i++){
        if(d[i] != 0){
            const SEXP jj = L[i];  // jj is temporary
            const SEXP kk = W[i];  // jj is temporary
            const Rcpp::CharacterVector players(jj);
            const Rcpp::NumericVector weights(kk);
            H[(weightedplayervector) makeweightedplayervector3(players, weights)] += d[i];
        } // else do nothing, a zero power 
    } // i loop closes
    return(H);
}

playerstrengths preparepmap3(const NumericVector &probs, const CharacterVector &pnames){// hamilton=0.7,vettel=0.2,button=0.1
    playerstrengths out;

    for(size_t i=0; i < (size_t) probs.length() ; i++){
        out[(string) pnames[i]] = probs[i];
    }
    return(out);
}

List makebrackets3(const hyper3 H){// Returns a list of players, eg list('a',c('a','b'),c('a','xt'))
    List out;
    size_t i;
    for(auto ih=H.begin(); ih != H.end(); ++ih){
        weightedplayervector nv = ih->first;
        weightedplayervector::iterator ion;
        CharacterVector t(nv.size());
        for(i=0 , ion=nv.begin() ; ion != nv.end() ; ion++, i++){
            t[i] = ion->first;
        }
        out.push_back(t);
    }
    return(out);
}

List makeweights3(const hyper3 H){// Returns a list of weights, eg list(1,1:2,2.2)
    List out;
    hyper3::const_iterator ih;
    size_t i;
    weightedplayervector::iterator ion;
    
     for(ih=H.begin(); ih != H.end(); ++ih){
        weightedplayervector nv = ih->first;
        Rcpp::NumericVector t(nv.size());
        for(i=0 , ion=nv.begin() ; ion != nv.end() ; ion++, i++){
            t[i] = ion->second;  // cf makebrackets3() [otherwise identical]
        }
        out.push_back(t);
     }
     return(out);
 }
 
NumericVector makepowers3(const hyper3 H){
    NumericVector out(H.size());   // data
    size_t i=0;
    
    for(auto it=H.begin(); it != H.end(); ++it){
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
    assert(L.size() == W.size());
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
    if(L1.size() > L2.size()){ // L1 is bigger, so iterate through L2
        for (auto it=h2.begin(); it != h2.end(); ++it){
            const weightedplayervector n = it->first;
            h1[n] += h2[n];  // the meat
        }
        return(retval3(h1));
    } else {  // L2 is bigger
        for (auto it=h1.begin(); it != h1.end(); ++it){
            const weightedplayervector n = it->first;
            h2[n] += h1[n];  
        }
        return(retval3(h2));
    }
}

//[[Rcpp::export]]
bool equality3(  // modelled on spray_equality()
               const List L1, const List W1, const NumericVector p1,
               const List L2, const List W2, const NumericVector p2
           ){
    hyper3 h1,h2;
    
    if(L1.size() != L2.size()){
        return false;
    }

    h1 = prepareL3(L1,W1,p1);
    h2 = prepareL3(L2,W2,p2);

    for (auto it=h1.begin(); it != h1.end(); ++it){
            const weightedplayervector n = it->first;
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
List accessor3(
              const List L,
              const List W,
              const NumericVector powers,
              const List Lwanted,
              const List Wwanted
              ){

    const hyper3 h=prepareL3(L,W,powers);
    hyper3 out;
    const int n=Lwanted.size();
    weightedplayervector b;

    for(size_t i=0; i < (size_t) n ; i++){
            const SEXP jj = Lwanted[i];
            const SEXP kk = Wwanted[i];
            const Rcpp::CharacterVector players(jj);
            const Rcpp::NumericVector weights(kk);
            assert(players.size() == weights.size());
            b = makeweightedplayervector3(players,weights);

            if(h.count(b)>0){
                out[b] = h.at(b);
            }
    }
    return retval3(out);
}
    
//[[Rcpp::export]]
List overwrite3(  // H1[] <- H2
                const List L1, const List W1, const NumericVector powers1,
                const List L2, const List W2, const NumericVector powers2
              ){

          hyper3 h1=prepareL3(L1,W1,powers1);
    const hyper3 h2=prepareL3(L2,W2,powers2);

    for(auto it=h2.begin(); it != h2.end(); ++it){
        const weightedplayervector nv = it->first;
        h1[nv] = h2.at(nv);
    }
    return retval3(h1);
}

//[[Rcpp::export]]
List assigner3(  // H[L] <- v
               const List L , const List W, const NumericVector p,
               const List L2, const List W2,
               const NumericVector value
              ){
    hyper3 h=prepareL3(L,W,p);
    weightedplayervector wp;
    const unsigned int n=L2.size();

    for(size_t i=0 ; i<n ; i++){
        wp.clear();
        const SEXP jj = L2[i];  // jj is temporary
        const SEXP kk = W2[i];  // kk is temporary

        const Rcpp::CharacterVector iv(jj);
        const Rcpp::NumericVector iw(kk);
        h[makeweightedplayervector3(iv,iw)] = value[i];
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
    playerstrengths psp = preparepmap3(probs,pnames);
    double out=0;
    double bracket_total=0;

    for (auto it=h.begin(); it != h.end(); ++it){
        const weightedplayervector n = it->first;
        bracket_total = 0;
        for (auto in=n.begin(); in != n.end(); ++in){
            bracket_total += psp[in->first] * (in->second); // the meat
                /*            bracket_total += psp[*in] * psw[*in]; */
        }
        out += log(bracket_total) * (it->second); // also the meat
    }
    return out;
}

double differentiate_single_independent3( // d(log-likelihod)/dp; see also differentiate_single_weight3()
                 const hyper3 h,
                 const unsigned int i,   // component to diff WRT [yes it's ugly]
                 const unsigned int n,   // p_1 + ...+p_n = 1
                 const NumericVector probs,
                 const CharacterVector pnames
                      ){
                                  
                 /* probs[0] == p_1, ..., probs[n-1] = p_n.  
                    probs[n-1] = p_n = fillup value. */

    weightedplayervector ps = makeweightedplayervector3(pnames,probs); //ps == "player strengths"
    assert(i > 0); 
    assert(i < n);  // sic; strict

    double out = 0;
    for (auto ih=h.begin(); ih != h.end(); ++ih){  // standard hyper3 loop
        weightedplayervector wp = ih->first;
  
        const double total_weight_diff = wp[(string) pnames[i  ]];
        const double total_weight_fill = wp[(string) pnames[n-1]];
        
        double bracket_total = 0;
        for (auto iw=wp.begin(); iw != wp.end(); ++iw){
            bracket_total += ps[iw->first] * (iw->second);  // the meat
        }

        const double power = ih->second;

        // the 'meat':
        out += (total_weight_diff-total_weight_fill)*power/bracket_total;
    }
    return out;
}



//[[Rcpp::export]]
List differentiate3(  // returns gradient of log-likelihood for hyper3 object
                const List &L,
                const List &W,
                const NumericVector &powers,
                const NumericVector &probs,
                const CharacterVector &pnames,
                const NumericVector &n
                  ){

    const unsigned int nn=n[0];
    NumericVector out(nn-1);  
    const hyper3 h=prepareL3(L,W,powers);

    for(size_t i=0; i<nn-1; i++){
        out[i] = differentiate_single_independent3(h,i,nn,probs,pnames);
    }
        return List::create(Named("grad_comp") =  out);
}


// following functions have a natural interpretation in terms of
// hyper3 objects [the differentiation is with respect to p, with
// weights held constant] but not coded up for hyper3 (function
// headers included below for reference)

/*
  double differentiate_single_alln( // d(log-likelihod)/dp
  double second_derivative( // evaluate terms of the lower-right block of the bordered Hessian

\\[[Rcpp::export]]
List hessian_lowlevel

\\[[Rcpp::export]]
List differentiate_n

*/
