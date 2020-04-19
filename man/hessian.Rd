\name{hessian}
\alias{hessian}
\alias{hessian_lowlevel}
\alias{is_ok_hessian}
\alias{bordered_hessian}
\alias{hessian_bordered}
\title{Bordered Hessian matrix}
\description{
Returns the bordered Hessian matrix for a \code{hyper2} object, by
default evaluated at the maximum likelihood estimate for \eqn{p}.

Function \code{is_ok_hessian()} calculates the bordered Hessian and
returns the result of the (bordered) second derivative test for the
maximum likelihood estimate\eqn{p} being a local maximum.

}
\usage{
hessian(H,probs=indep(maxp(H)))
hessian_lowlevel(L, powers, probs, n) 
is_ok_hessian(H)
}
\arguments{
  \item{H}{object of class \code{hyper2}}
  \item{L, powers, n}{Components of a \code{hyper2} object}
  \item{probs}{probability at which to evaluate the Hessian}
}
\details{

  Row and column names of the bordered Hessian are the \code{pnames()}
  of the \code{hyper2} object, plus \dQuote{\code{usc}} for \dQuote{Unit
    Sum Constraint}.

  The unit sum constraint borders can be added with idiom
  \code{magic::adiag(0,pad=1,hess)}, which might be preferable.

  Function \code{is_ok_hessian()} returns the result of the bordered
  second derivative test for the maximum likelihood estimate being a
  local maximum on the constraint hypersurface.  This is a
  generalization of the usual unconstrained problem, for which the test
  is Hessian's being negative-definite.

  Function \code{hessian_lowlevel()} is a low-level helper function that
  calls the \code{C++} routine.

  

}
\value{
Returns a square matrix
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{gradient}}}
\examples{
hessian(icons)
}
