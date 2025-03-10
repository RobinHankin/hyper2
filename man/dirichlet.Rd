\name{dirichlet}
\alias{dirichlet}
\alias{dirichlet3}
\alias{rdirichlet}
\alias{is.dirichlet}
\alias{rp_unif}
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
dirichlet3(powers, lambda=NULL)
GD(alpha, beta, beta0=0)
GD_wong(alpha, beta)
rdirichlet(n,H)
is.dirichlet(H)
rp_unif(n,H)
}
\arguments{
  \item{powers}{In function \code{dirichlet()} a (named) vector of powers}
  \item{alpha,beta}{A vector of parameters for the Dirichlet or
    generalized Dirichlet distribution}
  \item{beta0}{In function \code{GD()}, an arbitrary parameter}
  \item{H}{Object of class \code{hyper2}}
  \item{lambda}{Vector of weights in \code{dirichlet3()}}
  \item{n}{Number of observations}
}
\details{
  These functions are really convenience functions.

  Function \code{rdirichlet()} returns random samples drawn from a
  Dirichlet distribution using the gamma distribution.  If second
  argument \code{H} is a \code{hyper2} object, it is tested [with
  \code{is.dirichlet()}] for being a Dirichlet distribution.  If so,
  samples from it are returned.  If not, (e.g. \code{icons}), an error
  is given.  If \code{H} is not a \code{hyper2} object, it is
  interpreted as a (possibly named) vector of parameters
  \eqn{\alpha}{alpha} [\strong{not} a vector of powers].
  
  Function \code{rp_unif()} returns uniformly distributed vectors,
  effectively using \code{H*0}; but note that this uses Dirichlet
  sampling which is much faster and better than the Metropolis-Hastings
  functionality documented at \code{rp.Rd}.

  Functions \code{GD()} and \code{GD_wong()} return a likelihood
  function corresponding to the Generalized Dirichlet distribution as
  presented by Connor and Mosimann, and Wong, respectively.  In
  \code{GD_wong()}, \code{alpha} and \code{beta} must be named vectors;
  the names of \code{alpha} give the names of
  \eqn{x_1,\ldots,x_k}{x_1,...,x_k} and the last element of \code{beta}
  gives the name of \eqn{x_{k+1}}.

  Function \code{dirichlet3()} returns a \code{hyper3} object with
  weights \code{lambda}.  If \code{lambda} is length less than that of
  \code{powers}, it is padded with \code{1}s [so default \code{NULL}
  corresponds to unit weights, that is, a \code{hyper2} object].  A
  use-case is given in \code{inst/rock_paper_scissors_monster.Rmd}.
  
}

\references{
  \itemize{
    \item R. J. Connor and J. E. Mosimann 1969.  \dQuote{Concepts of
    independence for proportions with a generalization of the Dirichlet
    distribution}.  \emph{Journal of the American Statistical
    Association}, 64:194--206
    \item T.-T. Wong 1998. \dQuote{Generalized Dirichlet distribution in
    Bayesian Analysis}.  \emph{Applied Mathematics and Computation},
    97:165--181 }
}

\author{Robin K. S. Hankin} 

\note{
  A dirichlet distribution can have a term with zero power.  But
  this poses problems for \code{hyper2} objects as zero power brackets
  are dropped.

  Function \code{dirichlet3()} is a replacement for now removed function
  \code{pair3()}.

  Function \code{rdirichlet()} commits a very mild (but necessary in the
  absence of a working \code{dismat} package) violation of \code{disordR}
  discipline, as the columns of the returned matrix have the same order
  as \code{pnames(H)}

}

\seealso{\code{\link{hyper2}},\code{\link{rp}}} \examples{

x1 <- dirichlet(c(a=1,b=2,c=3))
x2 <- dirichlet(c(c=3,d=4))

x1+x2

H <- dirichlet(c(a=1,b=2,c=3,d=4))
rdirichlet(10,H)
colMeans(rdirichlet(1e4,H))

dirichlet3(c(fish=3,chips=2),lambda=1.8)
dirichlet3(c(x=6,y=5,z=2),1:3)

}
