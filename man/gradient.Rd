\name{gradient}
\alias{gradient}
\alias{gradientn}
\alias{hessian}
\alias{hessian_lowlevel}
\alias{is_ok_hessian}
\alias{bordered_hessian}
\alias{hessian_bordered}
\title{Differential calculus}
\description{

Given a \code{hyper2} object and a point in probability space,
function \code{gradient()} returns the gradient of the log-likelihood;
function \code{hessian()} returns the bordered Hessian matrix.  By
default, both functions are evaluated at the maximum likelihood estimate
for \eqn{p}, as given by \code{maxp()}.

}
\usage{
gradient(H, probs=indep(maxp(H)))
hessian(H,probs=indep(maxp(H)),border=TRUE,tidy=TRUE)
hessian_lowlevel(L, powers, probs, pnames,n) 
is_ok_hessian(H)
}
\arguments{
  \item{H}{A \code{hyper2} object}
  \item{L,powers,n}{Components of a \code{hyper2} object}
  \item{probs}{A vector of probabilities}
  \item{pnames}{Character vector of names}
  \item{border}{Boolean, with default \code{TRUE} meaning to return the
    bordered Hessian and \code{FALSE} meaning to return the Hessian
    (warning: this option does not respect the unit sum constraint)}
}

\details{

Function \code{gradient()} returns the gradient of the log-likelihood
function.  If the \code{hyper2} object is of size \eqn{n}, then argument
\code{probs} must be a vector of length \eqn{n-1}, typically
\code{indep(p)}; and the returned gradient is also a vector of length
\eqn{n-1}.  The hard part of this is accounting for the fillup value.
The function returns the derivative of the loglikelihood with respect to
the\eqn{n-1} independent components of
\eqn{\left(p_1,\ldots,p_n\right)}{(p_1,...,p_n)}, namely
\eqn{\left(p_1,\ldots,p_{n-1}\right)}{(p_1,...,p_n-1)}.  The fillup
value \eqn{p_n} is calculated as
\eqn{1-\left(p_1+\cdots + p_{n-1}\right)}{1-(p_1+...+p_n-1)}.

Function \code{gradientn()} returns the gradient of the loglikelihood
function but ignores the unit sum constraint.
If the \code{hyper2} object is of size \eqn{n}, then argument
\code{probs} must be a vector of length \eqn{n}, and the returned
gradient is also a vector of length \eqn{n}.  The last element of the
vector is not treated differently from the others; all \eqn{n} elements
are treated as independent.  The sum need not equal one.

Function \code{hessian()} returns the \dfn{bordered Hessian}, a matrix
of size \eqn{n+1\times n+1}{(n+1)*(n+1)}, which is useful when using
Lagrange's method of undetermined multipliers.  The first row and column
correspond to the unit sum constraint, \eqn{\sum p_1=1}{p_1+...+p_n=1}.
Row and column names of the matrix are the \code{pnames()} of the
\code{hyper2} object, plus \dQuote{\code{usc}} for \dQuote{Unit Sum
Constraint}.

The unit sum constraint borders could have been added with idiom
\code{magic::adiag(0,pad=1,hess)}, which might be preferable.

Function \code{is_ok_hessian()} returns the result of the second
derivative test for the maximum likelihood estimate being a local
maximum on the constraint hypersurface.  This is a generalization of the
usual unconstrained problem, for which the test is Hessian's being
negative-definite.

Function \code{hessian_lowlevel()} is a low-level helper function that
calls the \code{C++} routine.

}

\value{

Function \code{gradient()} returns a vector of length \eqn{n-1} with
entries being the gradient of the log-likelihood with respect to the
\eqn{n-1} independent components of
\eqn{\left(p_1,\ldots,p_n\right)}{(p_1,...,p_n)}, namely
\eqn{\left(p_1,\ldots,p_{n-1}\right)}{(p_1,...,p_n-1)}.  The fillup
value \eqn{p_n} is calculated as
\eqn{1-\left(p_1,\ldots,p_{n-1}\right)}{1-(p_1,...,p_n-1)}.

If argument \code{border} is \code{TRUE}, function \code{hessian()}
returns an \eqn{n+1}-by-\eqn{n+1} matrix of second derivatives; the
borders are as returned by \code{gradient()}.  If \code{border} is
\code{FALSE}, ignore the fillup value and return an \eqn{n}-by\eqn{n}
matrix.

}

\author{Robin K. S. Hankin}
\examples{

data(chess)
p <- c(1/2,1/3)
delta <- rnorm(2)/1e5  # delta needs to be quite small

deltaL  <- loglik(p+delta,chess) - loglik(p,chess)
deltaLn <- sum(delta*gradient(chess,p + delta/2))   # numeric

deltaL - deltaLn  # should be small [zero to first order]

hessian(icons)
is_ok_hessian(icons)

## Demostrate Hessian matrix of second derivatives:
## First, exact calculations:
p <- indep(equalp(icons))
dp <- rnorm(5)*1e-2
dpc <- fillup(dp,0)   # constrained dp
dL <- loglik(p+dp,icons) - loglik(p,icons)
hess <- hessian(icons,p,border=FALSE)

dL1 <- sum(gradient(icons,p)*dp)              # first order
dL2 <- dL1 + t(dpc) \%*\% hess \%*\% dpc/2    # second order
## above should be emulator::quad.form(dpc,hess)

print(c(exact_delta=dL,delta_1st_order=dL1,delta_2nd_order=dL2))
print(dL-c(first_order_error=dL1,second_order_error= dL2))

}
