\name{weights}
\alias{weight}
\alias{weights}
\alias{weights.hyper3}
\alias{setweight}
\alias{as.weight}
\alias{swait}
\title{Getting and setting the weights of symbols in \code{hyper3} objects}
\description{

Brackets of \code{hyper3} objects contain weighted probabilities; the
functionality documented here gets and sets the weights.

}
\usage{
weights.hyper3(object, ...)
as.weight(x)
setweight(x, index, value)
swait(x, ...)
}
\arguments{
  \item{x, object}{Objects of class \code{hyper3}}
  \item{index,value}{Vector of index names and values}
  \item{\dots}{named arguments corresponding to players to change weights}
}
\details{

  Suppose we have a \code{hyper3} object:

  \preformatted{
    > H <- hyper3_nv(list(c(a=1.2), c(b=1), c(a=1.2,b=1,c=1)), powers=c(3,4,-7))
    > H
    log( (a=1.2)^3 * (a=1.2, b=1, c=1)^-7 * (b=1)^4)
}


And we would like to change the weights of one of the players.  We can
do this in several ways:

Function \code{setweight()} alters the weight of every occurrence of a
set of players.  It is vectorised, so
\code{setweight(H,c("a","b"),88:89)} sets the weight of \code{a} to 88
and \code{b} to 89.  Example:

  \preformatted{
    > H <- hyper3_nv(list(c(a=1.2), c(b=1), c(a=1.2,b=1,c=1)), powers=c(3,4,-7))  # as before
    > setweight(H, "a",1.88)
    log( (a=1.88)^3 * (a=1.88, b=1, c=1)^-7 * (b=1)^4)
}

Experimental function \code{swait()} admits idiom such as \code{swait(H,
a=1.88)} to accomplish the same thing.

Replacement methods are defined, so \dQuote{\code{H["a"] <-
as.weight(3)}} will set the weight of every occurrence of player
\code{a} to 3.  If \code{H} is a \code{hyper2} object, it will be
coerced to \code{hyper3}.  

}
\value{
Generally return or deal with \code{hyper3} objects
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{hyper2}}}
\examples{

H <- hyper3_nv(list(c(a=1.2), c(b=1), c(a=1.2,b=1,c=1)), powers=c(3,4,-7))
H

setweight(H, "b", 1.88)


swait(H, b=1.88) # same thing


swait(H, a=1.1, b=2.2)

H |> swait(b=1.88)


race(letters[1:6]) |> swait(a=1.1, b=1.88)

}

