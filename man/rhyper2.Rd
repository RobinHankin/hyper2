\name{rhyper2}
\alias{rhyper2}
\title{Random \code{hyper2} objects}
\description{
Random \code{hyper2} loglikelihood functions, intended as quick
  \dQuote{get you going} examples
}
\usage{
rhyper2(n = 8, s = 5, pairs = TRUE, teams = TRUE, race = TRUE, pnames)
}
\arguments{
  \item{n}{Number of competitors, treated as even}
  \item{s}{Integer, Measure of the complexity of the log likelihood function}
  \item{pairs,teams,race}{Boolean, indicating whether or not to include
    different observations}
  \item{pnames}{Character vector of names, if missing interpret as
    \code{letters}; set to \code{NA} meaning no names}
}
\author{Robin K. S. Hankin}
\note{
  Function \code{rhyper2()} returns a likelihood function based on
  random observations.  To return a random probability vector drawn from
  a from a given (normalized) likelihood function, use \code{rp()}.
}
\seealso{\code{\link{rp}}}
\examples{

dotchart(maxp(rhyper2(n=8,s=10)))
dotchart(maxp(rhyper2(n=26,s=2,race=FALSE)))

}
