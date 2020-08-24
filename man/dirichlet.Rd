\name{dirichlet}
\alias{dirichlet}
\alias{rdirichlet}
\alias{is.dirichlet}
\alias{gd}
\alias{Dirichlet}
\alias{gd}
\alias{GD}
\alias{GD_wong}
\alias{Connor}
\alias{Mosimann}
\title{
Dirichlet distribution and generalizations
}
\description{
The Dirichlet distribution in likelihood (for p) form, including the
generalized Dirichlet distribution due to Connor and Mosimann
}
\usage{
dirichlet(powers, alpha)
GD(alpha, beta, beta0=0)
GD_wong(alpha, beta)
rdirichlet(n,H)
is.dirichlet(H)
}
\arguments{
  \item{powers}{In function \code{dirichlet()} a (named) vector of powers}
  \item{alpha,beta}{A vector of parameters for the Dirichlet or
    generalized Dirichlet distribution}
  \item{beta0}{In function \code{GD()}, an arbitrary parameter}
  \item{H}{Object of class \code{hyper2}}
  \item{n}{Number of observations}
}
\details{
  These functions are really convenience functions.

  Function \code{rdirichlet()} returns random samples drawn from a
  Dirichlet distribution.  If second argument \code{H} is a
  \code{hyper2} object, it is tested [with \code{is.dirichlet()}] for
  being a Dirichlet distribution.  If so, samples from it are returned.
  If not, (e.g. \code{icons}), then it is coerced to a \strong{uniform}
  distribution and samples from that returned, with a warning.  If
  \code{H} is not a \code{hyper2} object, it is interpreted as a vector
  of parameters \eqn{\alpha}{alpha} [\strong{not} a vector of powers].
  
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{hyper2}}}
\examples{

x1 <- dirichlet(c(a=1,b=2,c=3))
x2 <- dirichlet(c(c=3,d=4))

x1+x2

H <- dirichlet(c(a=1,b=2,c=3,d=4))
rdirichlet(10,H)
colMeans(rdirichlet(1e4,H))

}
