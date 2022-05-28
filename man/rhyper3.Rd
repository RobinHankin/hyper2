\name{rhyper3}
\alias{rracehyper3}
\alias{rwinner3}
\alias{rhyper3}
\alias{rpair3}
\title{Random hyper3 objects}
\description{

Various random hyper3 objects, in the context of the race metaphor.
}
\usage{
rwinner3(pn,ps)
rpair3(n=5,s=3,lambda=1.3)
rrace3(pn=c(a=2,b=4,c=2,d=1),ps=c(a=0.3, b=0.1,c=0.2,d=0.4))
rracehyper3(n=4,size=9,ps=NULL,races=3)
}
\arguments{
  \item{v}{Ranks in the form of a character vector}
  \item{nonfinishers}{Character vector (a set) showing players that did
    not finish}
  \item{a}{An ordertable}
  \item{pn}{A named integer vector showing numbers of each player}
  \item{ps}{A named vector showing strengths of each bplayer}
  \item{n,size,races}{Arguments specifying the complexity of the random
    \code{hyper3} object returned by \code{rracehyper3()}}
  \item{lambda}{Parameter}
  \item{...}{Arguments passed to \code{args2ordervec()}}
}
\details{

  These functions are intended to work with \code{hyper3} objects, as
  indicated by the \code{3} in their names.

  Function \code{args2ordervec()} takes named arguments and returns a
  sensible race result, essentially the same as the output of
  \code{rrace3()}.

  Function \code{ordervec2supp3()} takes character vector showing the
  order of finishing, and returns a generalized Plackett-Luce support
  function in the form of a \code{hyper3} object.  See the examples
  section below.

  Function \code{ordertable2supp3()} takes an order table (the canonical
  example is the constructors' formula 1 grand prix results, see
  \code{constructor.Rd} and returns a generalized Plackett-Luce support
  function in the form of a \code{hyper3} object.

  File \code{inst/javelin.Rmd} and \code{inst/race3.Rmd} show some
  use-cases for these functions.

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{rrank}},\code{\link{ordertable2supp}},\code{\link{ordertrans}}}
\examples{

rrace3() # moderate-sized default

## player numbers (two "a"s, four "b"s, two "c"s and a "d"):
pn <- c(a=2,b=4,c=2,d=1) 

ps <- c(a=0.3, b=0.1,c=0.2,d=0.4)  # player strengths    

rwinner3(pn,ps)              # winner, randomly chosen
rrace3(pn,ps)                 # A random race
ordervec2supp3(rrace3(pn,ps))  # a hyper3 object
rrace3()                        # a hyper3 object for multiple races

o <- args2ordervec(a=c(1,6,9), b=c(2,3,4), c=c(1.1,11.1))
H <- ordervec2supp3(o)
H
equalp.test(H)
 


}


