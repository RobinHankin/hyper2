\name{B}
\alias{B}
\alias{probability}
\alias{mgf}
\alias{mean}
\alias{Jacobian}
\alias{dhyper2}
\alias{dhyper2_e}
\alias{e_to_p}
\alias{p_to_e}
\alias{mean_hyper2}
\title{Normalizing constant for the hyperdirichlet distribution}
\description{
Uses numerical techniques for calculating the normalizing constant for the hyperdirichlet distribution
}
\usage{
B(H, disallowed=NULL, give=FALSE, ...)
probability(H, disallowed=NULL, ...)
mgf(H, powers, ...) 
dhyper2(P,H,...)
dhyper2_e(e,H,include.Jacobian=TRUE)
mean_hyper2(H, normalize=TRUE, ...)
Jacobian(e)
e_to_p(e)
p_to_e(p)
}
\arguments{
  \item{H}{Object of class hyper2}
  \item{powers}{Vector of length \code{dim(x)} whose elements are the
    powers of the expectation; see details section}
  \item{disallowed}{Function specifying a subset of the simplex
    over which to integrate; default \code{NULL} means to integrate over
    the whole simplex.  The integration proceeds over \code{p} with
    \code{disallowed(p)} evaluating to \code{FALSE}}
  \item{e,p}{Vectors; see details}
  \item{P}{A vector of probabilities with unit rowsums}
  \item{include.Jacobian}{Boolean, indicated whether or not to include
    the Jacobian transformation in the evaluation}
  \item{give}{Boolean, with default \code{FALSE} meaning to return the
    value of the integral and \code{TRUE} meaning to return the full
    output of \code{adaptIntegrate()}}
  \item{normalize}{Boolean, indicates whether return value of
    \code{mean_hyper2()} is normalized to have unit sum}
  \item{\dots}{Further arguments passed to \code{adaptIntegrate()}}
}
\details{
  \itemize{
    \item Function \code{B()} is the user-friendly version.  It accesses
    the \code{NC} slot.  If not \code{NA}, the value is returned; if
    \code{NA}, the normalizing constant is calculated using
    \code{adaptIntegrate()} of the \code{cubature()} package, via
    \code{calculate_B()}.
    
    \item Function \code{NC()} is not intended for the user.  It is used
    internally as an accessor method for the \code{NC} slot, and this
    value is returned indiscriminately.

    \item Function \code{calculate_B()} is the engine which actually
    does the work.  Observe how \eqn{p} is converted to \code{e} (by
    \code{e_to_p()}) and the integral proceeds over a hypercube.
    Function \code{dirichlet()} and \code{gd()} do not use this as the
    normalizing constant has an analytical expression and this is used
    instead.

    \item Function \code{probability()} gives the probability of an
    observation from a hyperdirichlet distribution satisfying
    \code{!disallowed(p)}.
    
    \item Function \code{mgf()} is the moment generating function,
    taking an argument that specifies the powers of \code{p} needed: the
    expectation of \eqn{\prod_{i=1}^n {p_i}^{{\rm powers}[i]}}{prod
    p^powers} is returned.

    \item Function \code{mean()} returns the mean value of the
    hyperdirichlet distribution.  This is computationally slow (consider
    \code{maximum_likelihood()} for a measure of central tendency).  The
    function takes a \code{normalize} argument, not passed to
    \code{adaptIntegrate()}: this is Boolean with \code{FALSE} meaning
    to return the value found by integration directly, and default
    \code{TRUE} meaning to normalize so the sum is exactly 1
  }
}
\value{
  Functions \code{B()}, \code{NC()}, \code{calculate_NC()} notionally
  return a scalar: the normalization constant

  Functions \code{mean()} and \code{mgf()} return a \eqn{k}-tuple

  Functions \code{is.proper()} and \code{validated()} return a Boolean

  Function \code{probability()} returns a scalar, a probability.
}
\author{Robin K. S. Hankin}

\note{

The adapt package is no longer available on CRAN; from 1.4-3, the
package uses \code{adaptIntegrate} of the cubature package.
}

\seealso{\code{\link{loglik}}}
\examples{

3+3
}
