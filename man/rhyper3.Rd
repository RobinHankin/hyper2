\name{rhyper3}
\alias{rracehyper3}
\alias{rrace3}
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
  \item{pn}{A named integer vector showing numbers of each player}
  \item{ps}{A named vector showing strengths of each player}
  \item{n,size,races,s}{Arguments specifying the complexity of the random
    \code{hyper3} object returned}
  \item{lambda}{Parameter}
}
\details{

  These functions are intended to work with \code{hyper3} objects, as
  indicated by the \code{3} in their names.

  \itemize{

    \item Function \code{rwinner3()} returns the winner of a race,
    chosen according to Plackett-Luce probabilities.
    
    \item Function \code{rpair3()} returns a \code{hyper3} object
    corresponding to repeated pairwise comparisons including a
    white-player advantage.
    
   \item Function \code{rwinner3()} returns a \code{hyper3} support
   function corresponding to repeated pairwise competition, with a white
   advantage represented by \code{lambda}.

   \item Function \code{rrace3()} returns a rank statistic corresponding
   to finishing order for a Plackett-Luce race.  The output can be
   passed to \code{ordervec2supp3()}.

   \item Function \code{rracehyper3()} returns a more complicated
   \code{hyper3} object corresponding to repeated races.

   \item Function \code{rhyper3()} returns an even more complicated
   \code{hyper3} object corresponding to repeated races and pairwise
   comparisons.
}

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

}
