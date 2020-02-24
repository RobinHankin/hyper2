\name{zapweak}
\alias{zapweak}
\title{Zap weak competitors}
\description{
Given a \code{hyper2} object, discard competitors with a small estimated
strength.}
\usage{
zapweak(H, minstrength = 1e-05, maxit, ...)
}
\arguments{
  \item{H}{Object of class \code{hyper2}}
  \item{minstrength}{Strength below which to discard competitors}
  \item{maxit}{Maximum number of iterations; if missing, use
    \code{size(H)-1}}
  \item{\dots}{Further arguments, passed to \code{}}
}
\details{

Iteratively discards weak players using \code{discard()}.  Uses
\code{maxp(..,n=1)} for efficiency.

}
\value{Returns a slimmed-down \code{hyper2} object.
\author{Robin K. S. Hankin}
\note{

}

\seealso{

}
\examples{


}