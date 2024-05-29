\name{loglik}
\alias{loglik}
\alias{loglik_single}
\alias{like_single_list}
\alias{like_series}
\title{Log likelihood functions}
\description{
  Returns a log-likelihood for a given \code{hyper2} or \code{hyper3}
  object at a specific point in probability space
}
\usage{
loglik(p, H, log = TRUE)
loglik_single(p,H,log=TRUE)
like_single_list(p,Lsub)
like_series(p,L,log=TRUE)

}
\arguments{
  \item{H}{An object of class \code{hyper2} or \code{hyper3}}
  \item{p}{A probability point.  See details}
  \item{log}{Boolean with default \code{TRUE} meaning to return the
    log-likelihood and \code{FALSE} meaning to return the likelihood}
  \item{L,Lsub}{A list of \code{hyper2} objects, or a list of list of
    \code{loglik} objects}
}
\details{

  Function \code{loglik()} is a straightforward likelihood function.  It
  can take a vector of length \code{n=size(H)} or \code{size(H)-1}; if
  given the vector
  \eqn{p=\left(p_1,\ldots,p_{n-1}\right)}{p=(p_1,...,p_{n-1})} it
  appends the fillup value, and then returns returns the (log)
  likelihood.

  If \code{p} is a matrix, the rows are interpreted as probability
  points.

  Function \code{loglik_single()} is a helper function that takes a
  single point in probability space.  Functions
  \code{like_single_list()} and \code{like_series()} are intended for
  use with \code{ggrl()}.

}
\note{

  \emph{Likelihood} is defined up to an arbitrary multiplicative
  constant.  Log-likelihood (also known as \emph{support}) is defined up
  to an arbitrary additive constant.

  If function \code{loglik()} is given a probability vector of length
  \code{n}, the vector must satisfy the unit sum constraint (up to a
  small tolerance).  Also, it must be a named vector with names
  (collectively) equal to the \code{pnames} of argument \code{H}.

  \preformatted{
  > pnames(chess)
  [1] "Topalov" "Anand"   "Karpov"  
  > loglik(c(Topalov=0.7,Anand=0.2,Karpov=0.1),chess)
  [1] -69.45364
  > loglik(c(Karpov=0.1,Topalov=0.7,Anand=0.2),chess)  # identical, just a different order
  [1] -69.45364
  }

  But if given a vector of length \code{n-1} [e.g. the value of
  \code{indep()}], then the names are ignored and the entries are
  interpreted as the BT strengths of \code{pnames(H)[seq_len(n-1)]}:

  \preformatted{
  > loglik(c(0.7,0.2),chess)
  [1] -69.45364
  > loglik(c(foo=0.7,bar=0.2),chess)  # names are ignored 
  [1] -69.45364
  }

  (the above applies for \code{H} a \code{hyper2} or \code{hyper3}
  object).

  Empty brackets are interpreted consistently: that is, zero whatever
  the probability vector (although the print method is not perfect).
  
  }
\author{Robin K. S. Hankin}
\seealso{\code{\link{maxp}}}
\examples{
data(chess)
loglik(c(1/3,1/3),chess)

loglik(rp(14,icons),icons)

\dontrun{ # takes too long
like_series(masterchef_maxp,masterchef)
like_series(indep(equalp(masterchef)),masterchef)
}

W <- hyper2(pnames=letters[1:6])
W1 <- ggrl(W, 'a', letters[2:5],'f')              # 24-element list
W2 <- ggrl(W, c('a','b'), c('c','d'),c('e','f'))  # 2^3=8 element list

like_single_list(rep(1/6,5),W1)      # information from first observation
like_series(rep(1/6,5),list(W1,W2))  # information from both observations

# hyper3 objects:
H3 <- ordervec2supp3(letters[c(1,2,3,3,2,1,2)])
loglik(c(a=1,b=2,c=3)/6,H3)
loglik(c(a=1,c=3,b=2)/6,H3) # identical
}
