\name{rp}
\alias{rp}
\title{Random samples from the prior of a \code{hyper2} object}
\description{
Uses Metropolis-Hastings to return random samples from the prior of a
\code{hyper2} object
}
\usage{
rp(n, H, startp = NULL, fcm = NULL, fcv = NULL, SMALL = 1e-06, l=loglik,...)
}
\arguments{
  \item{H}{Object of class \code{hyper2}}
  \item{n}{Number of samples}
  \item{startp}{Starting value for the Markov chain, with default
    \code{NULL} being interpreted as starting from the evaluate}
  \item{fcm,fcv}{Constraints as for \code{maxp()}}
  \item{SMALL}{Notional small value for numerical stability}
  \item{l}{Log-likelihood function with default \code{loglik()}}
  \item{...}{Further arguments, currently ignored}
}
\details{

  Uses the implementation of Metropolis-Hastings from the \code{MCE}
  package to sample from the posterior PDF of a \code{hyper2} object.
}
\value{
Returns a matrix, each row being an unit-sum observation.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{maxp}},\code{\link{loglik}}}
\examples{
rp(10,icons)
}
