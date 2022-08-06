\name{hyper3}
\alias{hyper3}
\alias{as.hyper3}
\alias{hyper3_to_hyper2}
\alias{as.namedvectorlist}
\alias{assign_lowlevel3}
\alias{hyper3_bw}
\alias{hyper3_nv}
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
\title{Weighted probability vectors: \code{hyper3} objects
  \Sexpr[stage=build,results=hide]{require(hyper2)}
}
\description{\loadmathjax

Objects of class \code{hyper3} are a generalization of \code{hyper2} objects
that allow the brackets to contain weighted probabilities.

As a motivating example, suppose two players with Bradley-Terry
strengths \mjseqn{p_1,p_2} play chess where we quantify the first-mover
advantage with a term \mjeqn{\lambda}{omitted}.  If \mjseqn{p_1} plays
white \mjseqn{a+b} times with \mjseqn{a} wins and \mjseqn{b} losses, and
plays black \mjseqn{c+d} times with \mjseqn{c} wins and \mjseqn{d}
losses, then a sensible likelihood function might be

\mjdeqn{
  \left(\frac{\lambda p_1}{\lambda p_1 + p_2}\right)^{a}
  \left(\frac{p_2        }{\lambda p_1 + p_2}\right)^{b}
  \left(\frac{p_1        }{p_1 + \lambda p_2}\right)^{c}
  \left(\frac{\lambda p_2}{p_1 + \lambda p_2}\right)^{d}
  }{omitted: see PDF}

If \mjseqn{a=1,b=2,c=3,d=4} and \mjeqn{\lambda=1.3}{omitted} appropriate
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

}
\usage{
hyper3(B = list(), W = list(), powers = 0, pnames)
hyper3_bw(B = list(), W = list(), powers = 0, pnames)
hyper3_nv(L=list(),powers=0,pnames)
}
\arguments{
  \item{B}{A list of brackets}
  \item{W}{A list of weights}
  \item{L}{A list of named vectors}
  \item{powers}{Numeric vector of powers}
  \item{pnames}{Character vector of player names}
}
\details{

  \itemize{

    \item Function \code{hyper3()} is the user-friendly creation method,
    which dispatches to helper functions \code{hyper3_bw()} and
    \code{hyper3_nv()} depending on its arguments.

    \item Function \code{hyper3_bw()} takes a list of brackets
    (character vectors) and a list of weights (numeric vectors)  and
    returns a \code{hyper3} object.

    \item Function \code{hyper2_nv()} takes a list of named vectors and
    returns a \code{hyper3} object.

    \item Function \code{evaluate3()} is a low-level helper function
    that evaluates a log-likelihood at a point in probability space.
    Don't use this: use the user-friendly \code{loglik()} instead, which
    dispatches to \code{evaluate3()}.

    \item Function \code{maxp3()} is a placeholder (it is not yet
    written).  But the intention is that it will maximize the
    log-likelihood of a \code{hype3} object over the Bradley Terry
    strengths \emph{and} any weights given.  This might not be possible
    as envisaged right now; I present some thoughts in
    \code{inst/kka.Rmd}.

    \item Function \code{list2nv()} converts a list of character vectors
    into a named vector suitable for use as argument \code{e} of
    function \code{cheering3()}.  It is used in
    \code{inst/global_liveability_ranking.Rmd}.
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


## Above two objects should be identical


a <- hyper3()  # default creation method [empty object]

a[c(p1=1.3)] <- 5
a[c(p1=1.3)] <- a[c(p1=1.3)] + 3
a[c(p1=1.3,p2=1)]  <- a[c(p1=1.3,p2=1)] -1
a

chess3  # representative simple hyper3 object

}

