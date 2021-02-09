\name{profile}
\alias{profile}
\alias{profile_support}
\alias{profile_likelihood}
\alias{profile_support_single}
\alias{profile_likelihood_single}
\alias{proflike}
\alias{profsupp}
\alias{profsup}
\title{Profile likelihood and support}
\description{
Given a support function, return a profile likelihood curve
}
\usage{
profsupp(H, i, p, relative=TRUE, ...)
profile_support_single(H, i, p, evaluate=FALSE, ...)
}
\arguments{
  \item{H}{hyper2 object}
  \item{i}{Element to be tested, character accepted}
  \item{p}{Strength of element \code{i}}
  \item{evaluate}{Boolean, with default \code{FALSE} meaning to return
    the maximal support for \code{p_i=p} and \code{TRUE} meaning to
    return the evaluate}
  \item{relative}{Boolean; if \code{TRUE} (default), return the support
    relative to the maximum support attained; if false, return the
    support as returned by \code{profile_support_single()}.}
  \item{...}{Arguments passed to \code{maxp()}}
}

\value{Returns the support at a particular value of \eqn{p_i},  or the
  evaluate conditional on \eqn{p_i}.}
\author{Robin K. S. Hankin}
\seealso{\code{\link{loglik}}}
\examples{

p <- seq(from=0.5,to=0.4,len=10)
u <- profsupp(icons,"NB",p)
plot(p,u-max(u))
abline(h=c(0,-2))
}