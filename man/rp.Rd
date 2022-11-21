\name{rp}
\alias{rp}
\title{Random samples from the prior of a \code{hyper2} object}
\description{
Uses Metropolis-Hastings to return random samples from the prior of a
\code{hyper2} object
}
\usage{
rp(n, H, startp = NULL, fcm = NULL, fcv = NULL, SMALL = 1e-06, l=loglik, fillup=TRUE, ...)
}
\arguments{
  \item{H}{Object of class \code{hyper2}}
  \item{n}{Number of samples}
  \item{startp}{Starting value for the Markov chain, with default
    \code{NULL} being interpreted as starting from the evaluate}
  \item{fcm,fcv}{Constraints as for \code{maxp()}}
  \item{SMALL}{Notional small value for numerical stability}
  \item{l}{Log-likelihood function with default \code{loglik()}}
  \item{fillup}{Boolean, with default \code{TRUE} meaning to return a
    matrix with the fillup value added, and column names matching the
    \code{pnames()} of argument \code{H}}
  \item{...}{Further arguments, currently ignored}
}
\details{

  Uses the implementation of Metropolis-Hastings from the \code{MCE}
  package to sample from the posterior PDF of a \code{hyper2} object.

  If the distribution is Dirichlet, use \code{rdirichlet()} to generate
  random observations: it is much faster, and produces serially
  independent samples.  To return \emph{uniform} samples, use
  \code{rp_unif()} (documented at \code{dirichlet.Rd}).
  
}
\value{Returns a matrix, each row being a unit-sum observation.}
\author{Robin K. S. Hankin}
\note{

  Function \code{rp()} a random sample from a given normalized
  likelihood function.  To return a random likelihood function, use
  \code{rhyper2()}.

  File \code{inst/ternaryplot_hyper2.Rmd} shows how to use
  \code{Ternary::ternaryPlot()} with \code{rp()}.

}
\seealso{\code{\link{maxp}},\code{\link{loglik}},\code{\link{dirichlet}},\code{\link{rhyper2}}}
\examples{
rp(10,icons)

plot(loglik(rp(30,icons),icons),type='b')
}
