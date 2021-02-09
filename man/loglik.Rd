\name{loglik}
\alias{loglik}
\alias{loglik_single}
\alias{like_single_list}
\alias{like_series}
\title{
Log likelihood functions
}
\description{
Returns a log-likelihood for a given hyper2 object at a specific point
}
\usage{
loglik(p, H, log = TRUE)
loglik_single(p,H,log=TRUE)
like_single_list(p,Lsub)
like_series(p,L,log=TRUE)

}
\arguments{
  \item{H}{An object of class \code{hyper2}}
  \item{p}{A probability point.  See details}
  \item{log}{Boolean with default \code{TRUE} meaning to return the
    log-likelihood and \code{FALSE} meaning to return the likelihood}
  \item{L,Lsub}{A list of \code{hyper2} objects, or a list of list of
    \code{loglik} objects}
}
\details{

Function \code{loglik()} is a straightforward likelihood function.  It
takes the vector
\eqn{p=\left(p_1,\ldots,p_{n-1}\right)}{p=(p_1,...,p_{n-1})} and returns
the (log) likelihood.  Note that the \sQuote{fillup} value is
automatically appended; the elements of \code{p} are (linearly!)
independent.

Function \code{size()} returns the (nominal) length \eqn{n} of
nonnegative vector \eqn{p=\left(p_1,\ldots,p_n\right)}{p=(p_1,...,p_n)}
where \eqn{p_1+\cdots+p_n=1}{p_1+...+p_n=1}.

If \code{p} is a matrix, the rows are interpreted as probability points.

Function \code{loglik_single()} is a helper function that takes a single
point in probability space.

Functions \code{like_single_list()} and \code{like_series()} are
intended for use with \code{ggrl()}.


}
\note{
  \emph{Likelihood} is defined up to an arbitrary multiplicative constant.

  Log-likelihood (also known as \emph{support}) is defined up to an
  arbitrary additive constant.

  Empty brackets are interpreted consistently: that is, zero whatever
  the probability vector (although the print method is not perfect).

  
  }
\author{Robin K. S. Hankin}
\seealso{\code{\link{maxp}}}
\examples{
data(chess)
loglik(c(1/3,1/3),chess)

data(masterchef)

a1 <- rep(1/13,12)                  # equal strengths
a2 <- indep(masterchef_maxp)        # MLE

like_series(a1,masterchef)
like_series(a2,masterchef)


W <- hyper2(pnames=letters[1:6])
W1 <- ggrl(W, 'a', letters[2:5],'f')              # 24-element list
W2 <- ggrl(W, c('a','b'), c('c','d'),c('e','f'))  # 2^3=8 element list

like_single_list(rep(1/6,5),W1)      # information from first observation
like_series(rep(1/6,5),list(W1,W2))  # information from both observations


}
