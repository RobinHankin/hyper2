\name{gradient}
\alias{gradient}
\alias{hessian}
\alias{hessian_lowlevel}
\alias{is_ok_hessian}
\alias{bordered_hessian}
\alias{hessian_bordered}
\title{Differential calculus}
\description{

Given a a \code{hyper2} object, function \code{gradient()} returns the
gradient of the log-likelihood as a function of
\eqn{(p_1,\ldots,p_n)}{p_1,...,p_n}, and function \code{hessian()}
returns the bordered Hessian matrix.  By default, both functions are
evaluated at the maximum likelihood estimate for \eqn{p}, as given by
\code{maxp()}.

}
\usage{
gradient(H, probs=indep(maxp(H)))
hessian(H,probs=indep(maxp(H)))
hessian_lowlevel(L, powers, probs, n) 
is_ok_hessian(H)
}
\arguments{
  \item{H}{A \code{hyper2} object}
  \item{L, powers, n}{Components of a \code{hyper2} object}
  \item{probs}{A vector of probabilities}
}

\details{

Function \code{gradient()} returns the gradient of the log-likelihood
function.  If the \code{hyper2} object is of size \eqn{n}, then argument
\code{probs} must be a vector of length \eqn{n-1}, and the returned
gradient is also a vector of length \eqn{n-1}.  The hard part of this is
accounting for the fillup value which may have a nonzero power.

Function \code{hessian()} returns the \dfn{bordered Hessian}, a matrix
of size \eqn{n+1\times n+1}{(n+1)*(n+1)}, which is useful when using
Lagrange's method of undetermined multipliers.  The first row and column
correspond to the unit sum constraint, \eqn{\sum p_1=1}{p_1+...+p_n=1}.
Row and column names of the matrix are the \code{pnames()} of the
\code{hyper2} object, plus \dQuote{\code{usc}} for \dQuote{Unit Sum
Constraint}.

The unit sum constraint borders could have been added with idiom
\code{magic::adiag(0,pad=1,hess)}, which might be preferable.

Function \code{is_ok_hessian()} returns the result of the bordered
second derivative test for the maximum likelihood estimate being a local
maximum on the constraint hypersurface.  This is a generalization of the
usual unconstrained problem, for which the test is Hessian's being
negative-definite.

Function \code{hessian_lowlevel()} is a low-level helper function that
calls the \code{C++} routine.

}

\value{
Function \code{gradient()} returns a vector with entries being the
gradient of the log-likelihood with respect to the \eqn{n-1} independent
components of \eqn{p}.  Function \code{hessian()} returns an \eqn{n+1}
by \eqn{n+1} matrix of second derivatives, bordered with constants
corresponding to the unit sum constraint.
}

\author{Robin K. S. Hankin}
\examples{

data(chess)
p <- c(1/2,1/3)
delta <- rnorm(2)/1e5  # delta needs to be quite small

deltaL  <- loglik(p+delta,chess) - loglik(p,chess)
deltaLn <- sum(delta*gradient(chess,p + delta/2))   # numeric

deltaL - deltaLn  # should be small

hessian(icons)
is_ok_hessian(icons)

}
