\name{ordertrans}
\alias{ordertrans}
\alias{ordertransplot}
\title{Order transformation}
\description{
Given an order vector, shuffle so that the players appear in a specified
order. 
}
\usage{
ordertrans(x,players)
ordertransplot(ox,oy,plotlims, ...)
}
\arguments{
  \item{x}{A (generalized) order vector}
  \item{players}{A character vector specifying the order in which the
    players will be listed; if missing, use \code{sort(names(x))}}
  \item{ox,oy}{Rank vectors}
  \item{plotlims}{Length two numeric vector giving x and y plot limits.
    If missing, use sensible default}
  \item{...}{Further arguments, passed to \code{plot()}}
}
\details{
  
  The best way to describe this function is with an example:

  \preformatted{
> x <- c(d=2,a=3,b=1,c=4)
> x
d a b c 
2 3 1 4 
}

In the above, we see \code{x} is an order vector showing that \code{d}
came second, \code{a} came third, \code{b} came first, and \code{c} came
fourth.  This is difficult to deal with because one has to search
through the vector to find a particular competitor, or a particular
rank.  This would be harder if the vector was longer.
If we wish to answer the question \dQuote{where did competitor \code{a}
  come?  where did \code{b} come?} we would want an \emph{order} vector
in which the competitors are in alphabetical order.  This is
accomplished by \code{ordertrans()}:

\preformatted{
> o <- ordertrans(x)
> o
a b c d 
3 1 4 2 
}

(this is equivalent to \code{o <- x[order(names(x))]}).  Object \code{o}
contains the same information as \code{x}, but presented differently.
This says that \code{a} came third, \code{b} came first, \code{c} came
fourth, and \code{d} came second.  In particular, the Plackett-Luce
order statistic is identical:

\preformatted{
> ordervec2supp(x) == ordervec2supp(o)
> [1] TRUE
}

There is a nice example of \code{ordertrans()} in
\code{inst/eurovision.Rmd}, and file \code{inst/ordertrans.Rmd} provides
further discussion and examples.

Function \code{ordertrans()} takes a second argument which allows the
user to arrange an order vector into the order specified.

Function \code{ordertrans()} also works in the context of \code{hyper3}
objects:

\preformatted{
x <- c(d=2,a=3,b=1,a=4)
x
d a b a 
2 3 1 4 
ordertrans(x)
a a b d 
3 4 1 2 
}
  
Object \code{x} shows that \code{d} came second, \code{a} came third and
fourth, and \code{b} came first.  We can see that \code{ordertrans()}
gives the same information in a more intelligible format.  This
functionality is useful in the context of \code{hyper3} likelihood
functions.
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
x
ordertrans(x,letters[1:9])

o <- skating_table[,1]
names(o) <- rownames(skating_table)
o
ordertrans(o)

ordertrans(sample(icons_maxp),icons)


rL <- volvo_maxp   # rL is "ranks Likelihood"
rL[] <- rank(-volvo_maxp)

r1 <- volvo_table[,1]  # ranks race 1
names(r1) <- rownames(volvo_table)
ordertransplot(rL,r1,xlab="likelihood rank, all races",ylab="rank, race 1")

}
