#include "hyperdirichlet2.h"

hyper2 prepareL(const List &L, const NumericVector &d){
    hyper2 H;   
    bracket b;
    unsigned int i,j;
    const unsigned n=L.size();
    
    for(i=0; i<n ; i++){
        if(d[i] != 0){
            b.clear();

            const SEXP jj = L[i];  // jj is temporary
            const Rcpp::CharacterVector names(jj);
            for(j=0 ; j<(unsigned int) names.size(); j++){
                b.insert((string) names[j]);
            }
            H[b] += d[i];  // the meat
        } // else do nothing, a zero power 
    } // i loop closes
    return(H);
}

psub preparepmap(const NumericVector &probs, const CharacterVector &pnames){
    psub out;

    for(int i=0; i<probs.length() ; i++){
        out[(string) pnames[i]] = probs[i];
    }
    return(out);
}

List makebrackets(const hyper2 H){  // takes a hyper2, returns the brackets
    List out;
    hyper2::const_iterator ih;
    
    for(ih=H.begin(); ih != H.end(); ++ih){
        out.push_back(ih->first);
    }
    return(out);
}

NumericVector makepowers(const hyper2 H){  // takes a hyper2, returns powers
    NumericVector out(H.size());   // data
    unsigned int i=0;
    hyper2::const_iterator it;   // it iterates through a hyper2 object
    
    for(it=H.begin(); it != H.end(); ++it){
        out(i++) = it->second;   // initialize-and-fill is more efficient than  out.push_back(it->second) 
    }
    return(out);
}

List retval(const hyper2 &H){  // used to return a list to R
    
        return List::create(Named("brackets") =  makebrackets(H),
                            Named("powers")   =  makepowers(H)
                            );
}

// [[Rcpp::export]]
List identityL(const List &L, const NumericVector &p){
    const hyper2 out = prepareL(L,p);
    return retval(out);
}
 
//[[Rcpp::export]]
List addL(
          const List L1, const NumericVector p1,
          const List L2, const NumericVector p2  // p==powers
          ){
    hyper2 h1 = prepareL(L1,p1);
    hyper2 h2 = prepareL(L2,p2);
    hyper2::const_iterator it;
    if(L1.size() > L2.size()){ // L1 is bigger, so iterate through L2
        for (it=h2.begin(); it != h2.end(); ++it){
            const bracket b = it->first;
            h1[b] += h2[b];  
        }
        return(retval(h1));
    } else {  // L2 is bigger
        for (it=h1.begin(); it != h1.end(); ++it){
            const bracket b = it->first;
            h2[b] += h1[b];  
        }
        return(retval(h2));
    }
}

//[[Rcpp::export]]
bool equality(  // modelled on spray_equality()
          const List L1, const NumericVector p1,
          const List L2, const NumericVector p2  // p==powers
           ){
    hyper2 h1,h2;
    hyper2::const_iterator it;
    
    if(L1.size() != L2.size()){
        return false;
    }

    h1 = prepareL(L1,p1);
    h2 = prepareL(L2,p2);

    for (it=h1.begin(); it != h1.end(); ++it){
            const bracket b = it->first;
            if(h1[b] != h2[b]){
                return false;
            } else {
                h2.erase(b);
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
              const NumericVector powers,
              const List Lwanted
              ){

    const hyper2 h=prepareL(L,powers);
    hyper2 out;
    bracket b;
    const unsigned int n=Lwanted.size();
    unsigned int i,j;

    for(i=0; i<n ; i++){
            b.clear();
            const SEXP jj = Lwanted[i]; // jj is temporary
            const Rcpp::CharacterVector names(jj);

            for(j=0 ; j<(unsigned int) names.size(); j++){
                b.insert((string) names[j]);
            }
            
            if(h.count(b)>0){
                out[b] = h.at(b);
            }
    }
    return retval(out);
}
    
//[[Rcpp::export]]
List overwrite(  // H1[] <- H2
              const List L1, const NumericVector powers1,
              const List L2, const NumericVector powers2
              ){

          hyper2 h1=prepareL(L1,powers1);
    const hyper2 h2=prepareL(L2,powers2);
    hyper2::const_iterator it;

    for(it=h2.begin(); it != h2.end(); ++it){
        const bracket b = it->first;
        h1[b] = h2.at(b);
    }
    return retval(h1);
}

//[[Rcpp::export]]
List assigner(  // H[L] <- v
            const List L, const NumericVector p,
            const List L2,
            const NumericVector value
              ){
    hyper2 h=prepareL(L,p);
    bracket b;
    hyper2::const_iterator it;
    const unsigned int n=L2.size();
    unsigned int i,j;


    for(i=0 ; i<n ; i++){
        b.clear();
        const SEXP jj = L2[i];  // jj is temporary
        const Rcpp::CharacterVector iv(jj);
        for(j=0 ; j<(unsigned int) iv.size(); j++){
            b.insert((string) iv[j]);
        } 
        h[b] = value[i]; // RHS might be zero in which case this entry is deleted from h
    }
    return retval(h);
}
                     
//[[Rcpp::export]]
double evaluate(  // returns log-likelihood
                const List &L,
                const NumericVector &powers,
                const NumericVector &probs,
                const CharacterVector &pnames
                  ){

    const hyper2 h = prepareL(L,powers);
    psub ps = preparepmap(probs,pnames);
    
    hyper2::const_iterator it;
    double out=0;
    double bracket_total=0;
    bracket::const_iterator ib;

    for (it=h.begin(); it != h.end(); ++it){
        const bracket b = it->first;
        bracket_total = 0;
        for (ib=b.begin(); ib != b.end(); ++ib){
            bracket_total += ps[*ib];  //NB off-by-one error not a problem!
        }
        out += log(bracket_total) * (it->second);
    }
    return out;
}

double differentiate_single_independent( // d(log-likelihod)/dp
                 const hyper2 h,
                 unsigned int i,   // component to diff WRT [yes it's ugly]
                 const unsigned int n,   // p_1 + ...+p_n = 1
                 const NumericVector probs,
                 const CharacterVector pnames
                 
                 
                 /* probs[0] == p_1, ..., probs[n-1] = p_n.  
                    probs[n-1] = p_n = fillup value.

                    bracket b has elements in the range 1,2,...,n.
                    If, say, 3 is an element of b, then this
                    corresponds to p_3==probs[2].  In particular, if
                    n\in b, then this is the fillup.
                  */
                      ){

    hyper2::const_iterator it;
    bracket::const_iterator ib;
    double out;
    double bracket_total;
    unsigned int no_of_diff_terms; // number of p_i terms in the bracket
    unsigned int no_of_fill_terms; // number of p_n terms (=fillup) in the bracket

    psub ps = preparepmap(probs,pnames);
    
    assert(i > 0); 
    assert(i < n);  // sic; strict

    out = 0;
    for (it=h.begin(); it != h.end(); ++it){  // standard hyper2 loop
        const bracket b = it->first;

        no_of_diff_terms = b.count((string) pnames[i]);
        no_of_fill_terms = b.count((string) pnames[n-1]);
            
        if(no_of_diff_terms == no_of_fill_terms){continue;} // bracket has same number (maybe 0!) of diff and fillups

        bracket_total = 0;
        for (ib=b.begin(); ib != b.end(); ++ib){
            bracket_total += ps[*ib];  //NB off-by-one error not a problem
        }

        const double power = it->second;
        // the 'meat':
        out += no_of_diff_terms*power/bracket_total;
        out -= no_of_fill_terms*power/bracket_total; 
    }
    return out;
}

double differentiate_single_alln( // d(log-likelihod)/dp
                 const hyper2 h,
                 unsigned int i,   // component to diff WRT
                 const unsigned int n,   // p_1 + ...+p_n  not necessarily =1
                 const NumericVector probs,
                 const CharacterVector pnames
                 /* basically the same as
                    differentiate_single_independent(), but we ignore
                    the special nature of the fillup entry
                 */
                      ){

    hyper2::const_iterator it;
    bracket::const_iterator ib;
    double out;
    double bracket_total;
    psub ps = preparepmap(probs,pnames);

    assert(i > 0); 
    assert(i < n);
    out = 0;
    for (it=h.begin(); it != h.end(); ++it){  // standard hyper2 loop
        const bracket b = it->first;
        const int no_of_diff_terms = b.count((string) pnames[i]);
        bracket_total = 0;
        if(no_of_diff_terms > 0){ // bracket has i in it
            for (ib=b.begin(); ib != b.end(); ++ib){
                bracket_total += ps[*ib]; // Off-by-one error not an issue
            }
            
            // the 'meat':
            double power = it->second;
            out += power/bracket_total;
        }
    }
    return out;
}

double second_derivative( // evaluate terms of the lower-right block of the bordered Hessian
                 const hyper2 h,
                 const int i,   // components to diff WRT
                 const int j,   // components to diff WRT
                 const NumericVector probs,
                 const CharacterVector pnames
                      ){

    hyper2::const_iterator it;
    bracket::const_iterator ib;
    double out=0;
    psub ps = preparepmap(probs,pnames);

    for (it=h.begin(); it != h.end(); ++it){  // standard hyper2 loop
        const bracket b = it->first;
        if( (b.count((string) pnames[i])>0) && (b.count((string) pnames[j])>0) ){
            double bracket_total=0;
            for (ib=b.begin(); ib != b.end(); ++ib){
                bracket_total += ps[*ib];  // off-by-one not an issue
            }
            out -= (it->second)/(bracket_total*bracket_total);
        }
    }  // hyper2 iterator loop closes
    return out;
} // function second_derivative() closes

//[[Rcpp::export]]
List hessian_lowlevel(
             const List &L,
             const NumericVector &powers,
             const NumericVector &probs,
             const CharacterVector &pnames,
             const NumericVector &n
               ){

    const int nn=n[0];
    NumericVector out(nn*nn);
    const hyper2 h=prepareL(L,powers);
    int r=0;
    psub ps = preparepmap(probs,pnames);
    
    for(int i=0; i<nn-1; i++){
        for(int j=0; j<nn-1; j++){
            out[r++] = second_derivative(h,i,j,probs,pnames);
        }
    }
    return List::create(Named("block_hessian_components") =  out);
}

//[[Rcpp::export]]
List differentiate(  // returns gradient of log-likelihood
                const List &L,
                const NumericVector &powers,
                const NumericVector &probs,
                const CharacterVector &pnames,
                const NumericVector &n
                  ){

    unsigned int i;
    const unsigned int nn=n[0];
    NumericVector out(nn-1);  
    const hyper2 h=prepareL(L,powers);

    for(i=0; i<nn-1; i++){
        out[i] = differentiate_single_independent(h,i,nn,probs,pnames);
    }
        return List::create(Named("grad_comp") =  out);
}
//[[Rcpp::export]]
List differentiate_n(  // returns gradient of log-likelihood
                const List &L,
                const NumericVector &powers,
                const NumericVector &probs,
                const CharacterVector &pnames,
                const NumericVector &n
                  ){

    unsigned int i;
    const unsigned int nn=n[0];
    NumericVector out(nn);  

    const hyper2 h=prepareL(L,powers);

    for(i=0; i<nn; i++){  // differs from differentiate()
        out[i] = differentiate_single_alln(h,i,nn,probs,pnames);
    }
        return List::create(Named("grad_comp") =  out);
}
