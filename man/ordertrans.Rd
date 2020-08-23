\name{ordertrans}
\alias{ordertrans}
\title{Order transformation}
\description{
Given an order vector, shuffle so that the players appear in a specified
order. 
}
\usage{ordertrans(x,players)}
\arguments{
  \item{x}{A (generalized) order vector}
  \item{players}{A character vector specifying the order in which the
    players will be listed; if a named vector, use the names}
}

\details{

  The best way to describe this function is with an example:

  \preformatted{

> x <- c(d=2,a=3,b=1,c=4)
> x
d a b c 
2 3 1 4 
> o <- ordertrans(x,letters[1:4])
> o
a b c d 
3 1 4 2 
> ordervec2supp(x) == ordervec2supp(o)
> [1] TRUE
}

In the above, we see \code{x} is an order vector showing that \code{d}
came second, \code{a} came third, \code{b} came first, and \code{c} came
fourth.  Object \code{o} contains the same information but presented
differently.  This says that \code{a} came third, \code{b} came first,
\code{c} came fourth, and \code{d} came second.
}
\value{Returns a named vector}
\author{Robin K. S. Hankin}
\note{

  The argument to \code{ordertrans()} is technically an order vector
  because it answers the question \dQuote{where did the first-named
  competitor come?} (see the discussion at \code{rrank.Rd}).  But it is
  not a helpful order vector because you have to go searching through
  the names---which can appear in any order---for the competitor you are
  interested in.  I guess \dQuote{generalised order vector} might be a
  better description of the argument.
}
\seealso{\code{\link{rrank}}}
\examples{

x <- c(e=4L,a=7L,c=6L,b=1L,f=2L,g=3L,h=5L,i=8L,d=9L)
ordertrans(x,letters[1:9])

o <- skating_table[,1]
n <- rownames(skating_table)
names(o)  <- n

ordertrans(o, sort(n))

}

