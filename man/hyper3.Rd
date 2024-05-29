\name{hyper3}
\alias{hyper3}
\alias{as.hyper3}
\alias{hyper3_to_hyper2}
\alias{as.namedvectorlist}
\alias{assign_lowlevel3}
\alias{hyper3_bw}
\alias{hyper3_nv}
\alias{hyper3_m}
\alias{is.hyper3}
\alias{is_ok_weightedplayers}
\alias{is_valid_hyper3}
\alias{loglik_single_redundant}
\alias{maxp3}
\alias{list2nv}
\alias{overwrite_lowlevel3}
\alias{powers<-.hyper3}
\alias{pnames<-.hyper3}
\alias{weights}
\alias{weights.hyper3}
\alias{setweight}
\alias{as.weight}
\title{Weighted probability vectors: \code{hyper3} objects}
\description{

Objects of class \code{hyper3} are a generalization of \code{hyper2} objects
that allow the brackets to contain weighted probabilities.

As a motivating example, suppose two players with Bradley-Terry
strengths \eqn{p_1,p_2} play chess where we quantify the first-mover
advantage with a term \eqn{\lambda}{omitted}.  If \eqn{p_1} plays
white \eqn{a+b} times with \eqn{a} wins and \eqn{b} losses, and
plays black \eqn{c+d} times with \eqn{c} wins and \eqn{d}
losses, then a sensible likelihood function might be

\deqn{
  \left(\frac{\lambda p_1}{\lambda p_1 + p_2}\right)^{a}
  \left(\frac{p_2        }{\lambda p_1 + p_2}\right)^{b}
  \left(\frac{p_1        }{p_1 + \lambda p_2}\right)^{c}
  \left(\frac{\lambda p_2}{p_1 + \lambda p_2}\right)^{d}
  }{omitted: see PDF}

If \eqn{a=1,b=2,c=3,d=4} and \eqn{\lambda=1.3}{omitted} appropriate
package idiom might be:

\preformatted{

H <- hyper3()
H[c(p1=1.3)]      \%<>\% inc(1) # a=1
H[c(p2=1)]        \%<>\% inc(2) # b=2
H[c(p1=1.3,p2=1)] \%<>\% dec(3) # a+b=1+2=3
H[c(p1=1)]        \%<>\% inc(3) # c=3
H[c(p2=1.3)]      \%<>\% inc(4) # d=4
H[c(p1=1,p2=1.3)] \%<>\% dec(7) # c+d=3+4=7
H
> log( (p1=1)^3 * (p1=1, p2=1.3)^-7 * (p1=1.3)^1 * (p1=1.3, p2=1)^-3 *
(p2=1)^2 * (p2=1.3)^4)
}

The general form of terms of a \code{hyper3} object would be
\eqn{\left(w_1p_1+\cdots+w_rp_r\right)^{\alpha}}{omitted}; the
complete object would be

\deqn{
  \mathcal{L}\left(p_1,\ldots,p_n\right)=
  \prod_{j=1}^N\left(\sum_{i=1}^n
  w_{ij}p_i\right)^{\alpha_i}
  }{omitted: see PDF}

where we understand that \eqn{p_n=1-\sum_{i=1}^{n-1}p_i}{omitted};
many of the weights might be zero.  We see that the weights
\eqn{w_{ij}}{w_ij} may be arranged as a matrix and this form is taken
by function \code{hyper3_m()}.

}
\usage{
hyper3(B = list(), W = list(), powers = 0, pnames)
hyper3_bw(B = list(), W = list(), powers = 0, pnames)
hyper3_nv(L=list(),powers=0,pnames)
hyper3_m(M,p,stripzeros=TRUE)
}
\arguments{
  \item{B}{A list of brackets}
  \item{W}{A list of weights}
  \item{L}{A list of named vectors}
  \item{powers}{Numeric vector of powers}
  \item{pnames}{Character vector of player names}
  \item{M}{Matrix of weights, column names being player names}
  \item{p}{Vector of powers, length equal to \code{ncol(M)}}
  \item{stripzeros}{Boolean with default \code{TRUE} meaning to silently
    remove all-zero rows of \code{M}}
}
\details{

  \itemize{

    \item Function \code{hyper3()} is the user-friendly creation method,
    which dispatches to a helper function depending on its arguments.

    \item Function \code{hyper3_bw()} takes a list of brackets
    (character vectors) and a list of weights (numeric vectors)  and
    returns a \code{hyper3} object.

    \item Function \code{hyper3_nv()} takes a list of named vectors and
    returns a \code{hyper3} object.

    \item Function \code{hyper3_m()} takes a matrix with rows being the
    brackets (entries are weights) and a numeric vector of powers.

    \item Function \code{evaluate3()} is a low-level helper function
    that evaluates a log-likelihood at a point in probability space.
    Don't use this: use the user-friendly \code{loglik()} instead, which
    dispatches to \code{evaluate3()}.

    \item Function \code{maxp3()} is a placeholder (it is not yet
    written).  But the intention is that it will maximize the
    log-likelihood of a \code{hyper3} object over the Bradley Terry
    strengths \emph{and} any weights given.  This might not be possible
    as envisaged right now; I present some thoughts in
    \code{inst/kka.Rmd}.

    \item Function \code{list2nv()} converts a list of character vectors
    into a named vector suitable for use as argument \code{e} of
    function \code{cheering3()}.  It is used in
    \code{inst/global_liveability_ranking.Rmd}.

    \item Function \code{as.namedvectorlist()} takes a \code{hyper3}
    object and returns a disoRdered list of named vectors corresponding
    to the brackets and their weights.

    \item Function \code{setweight()} alters the weight of every
    occurrence of a set of players.  It is vectorised, so
    \code{setweight(H,c("a","b"),88:89)} sets the weight of \code{a} to
    88 and \code{b} to 89.  Replacement methods are defined, so
    \dQuote{\code{H["a"] <- as.weight(3)}} will set the weight of every
    occurrence of player \code{a} to 3.  If \code{H} is a \code{hyper2}
    object, it will be coerced to \code{hyper3}.

  }
}
\value{
Generally return or deal with \code{hyper3} objects
}
\author{Robin K. S. Hankin}
\note{
Functionality for \code{hyper3} objects is generally indicated by adding
a \dQuote{\code{3}} to function names, eg \code{gradient()} goes to
\code{gradient3()}.
}
\seealso{\code{\link{hyper2}}}

\examples{


hyper3(B=list("a",c("a","b"),"b"),W=list(1.2,c(1.2,1),1),powers=c(3,4,-7))
hyper3(list(c(a=1.2),c(b=1),c(a=1.2,b=1)),powers=c(3,4,-7))
## Above two objects should be identical.

## Third method, send a matrix:
M <- matrix(rpois(15,3),5,3)
colnames(M) <- letters[1:3]
hyper3(M,c(2,3,-1,-5,1))   # second argument interpreted as powers



## Standard way to generate a hyper3 object is to create an empty object
## and populate it using the replacement methods:

a <- hyper3()  # default creation method [empty object]

a[c(p1=1.3)] <- 5
a[c(p2=1  )] <- 2
a[c(p1=1.3,p2=1)] <- -7
a

chess3  # representative simple hyper3 object

H1 <- rankvec_likelihood(letters[sample(6)])
H2 <- rankvec_likelihood(letters[sample(6)])
H1["a"] <- as.weight(1.2)         # "a" has some disadvantage in H1
H1[c("b","c")] <- as.weight(2:3)  # "b" and "c" have some advantage in H1
H2[c("c","d")] <- as.weight(1.5)  # "c" and "d" have some advantage in H2
H1+H2

}

