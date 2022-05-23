\name{hyper3}
\alias{hyper3}
\alias{as.hyper3}
\alias{Ops.hyper3}
\alias{accessor3}
\alias{addL3}
\alias{char2nv}
\alias{hyper3_to_hyper2}
\alias{as.namedvectorlist}
\alias{assign_lowlevel3}
\alias{assigner3}
\alias{differentiate3}
\alias{equality3}
\alias{evaluate3}
\alias{hyper3_add}
\alias{hyper3_bw}
\alias{hyper3_equal}
\alias{hyper3_nv}
\alias{identityL3}
\alias{is.hyper3}
\alias{is_ok_weightedplayers}
\alias{is_valid_hyper3}
\alias{loglik_single_redundant}
\alias{maxp3}
\alias{overwrite3}
\alias{overwrite_lowlevel3}
\alias{pnv}
\alias{powers<-.hyper3}
\alias{pnames<-.hyper3}
\alias{print.hyper3}
\alias{rhyper3}
\alias{rpair3}
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
}
\arguments{
  \item{B}{A list of brackets}
  \item{W}{A list of weights}
  \item{powers}{Numeric vector of powers}
  \item{pnames}{Character vector of player names}
}
\details{
Details here
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




\runExamples{
a <- hyper3()
a[c(p1=1.3)] <- a[c(p1=1.3)] + 1
a[c(p1=1.3,p2=1)]  <- a[c(p1=1.3,p2=1)] -1
a
}
