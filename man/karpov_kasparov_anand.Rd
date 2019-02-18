\name{karpov_kasparov_anand}
\alias{karpov_kasparov_anand}
\docType{data}
\title{Karpov, Kasparov, Anand}
\description{
A likelihood function for the strengths of three chess players: Karpov,
Kasparov, and Anand}
\usage{data(karpov_kasparov_anand)}
\details{
  The strengths of chess players may be assessed using the generalized
  Bradley-Terry model.  The \code{karpov_kasparov_anand} likelihood
  function allows one to estimate the players' strengths, propensity to
  draw, and also the additional strength conferred by playing white.

  The likelihood function is identical to object \code{H}, created by
  file \code{inst/karpov_kasparov_anand.R}, which is heavily documented.

  The reason that the players are different from those in the
  \code{chess} dataset is that the original data does not seem to be
  available any more.
}
\seealso{\code{\link{chess}}}
\examples{
data(karpov_kasparov_anand)
maxp(karpov_kasparov_anand)
}
\keyword{datasets}
