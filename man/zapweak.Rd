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
  \item{\dots}{Further arguments, passed to \code{maxp()}}
}
\details{
Iteratively discards the weakest player (if the estimated strength is
less than \code{minstrength}) using \code{discard_flawed()}.
\code{maxp(..,n=1)} for efficiency.  
}
\value{Returns a slimmed-down \code{hyper2} object with weak players
  removed.
}
\note{

  This function is experimental and appears to be overly aggressive.
  For some likelihood functions \code{zapweak()} removes \emph{all} the
  players.

  I now think that there is no consistent way to remove weaker players
  from a likelihood function.  I think the only way to do it is to look
  at the dataset that generates the likelihood function, somehow weed
  out the players with the poorest performance, and generate a new
  likelihood function without them.

}

\seealso{\code{\link{discard_flawed}},\code{\link{maxp}}}
\author{Robin K. S. Hankin}
\examples{
 zapweak(icons)        # removes noone
# zapweak(rowing)      # removes everyone...

}

