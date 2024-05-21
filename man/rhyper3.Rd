\name{rhyper3}
\alias{rracehyper3}
\alias{rrace3}
\alias{rwinner3}
\alias{rhyper3}
\alias{rpair3}
\title{Random hyper3 objects}
\description{

  Various random hyper3 objects, in the context of the race metaphor.
  They return \dQuote{get you going} examples of \code{hyper3} objects.
  The defaults correspond to simple but non-trivial with straightforward
  interpretations.

  The defaults are

  \preformatted{

 pn: c(a=2,   b=4,   c=2,   d=1  )  # numbers (two "a"s, four "b"s etc)
 ps: c(a=0.3, b=0.1, c=0.2, d=0.4)  # strengths
}

  
}
\usage{
rwinner3(pn=c(a=2,b=4,c=2,d=1),ps=c(a=0.3, b=0.1,c=0.2,d=0.4))
rpair3(n=5,s=3,lambda=1.3)
rrace3(pn=c(a=2,b=4,c=2,d=1),ps=c(a=0.3, b=0.1,c=0.2,d=0.4))
rracehyper3(n=4,size=9,ps=NULL,races=3)
rhyper3(n=5,s=4,type='race',...)
}
\arguments{
  \item{pn}{A named integer vector showing numbers of each type of player}
  \item{ps}{A named vector showing strengths of each type of player}
  \item{n,size,races,s,type}{Arguments specifying the complexity 
    of the random \code{hyper3} object returned.  See details}
  \item{lambda}{Parameter}
  \item{...}{Further arguments passed to \code{rracehyper3()} or
    \code{rpair3()}}
}
\details{

  These functions return \code{hyper3} objects, as indicated by the
  \code{3} in their names.

  \itemize{

    \item Function \code{rwinner3()} is a low-level helper function that
    takes a player number argument \code{pn}, and a player strength
    argument \code{ps}.  It performs an \emph{in silico} race,  and
    returns the (name of) the winner, chosen randomly from a field of
    runners with appropriate strengths.  It is used repeatedly by
    \code{rrace3()} to select a winner from the diminishing pool of
    still-running players.

    \item Function \code{rpair3()} returns a \code{hyper3} object
    corresponding to repeated pairwise comparisons including a
    white-player advantage represented by \code{lambda}.
    
   \item Function \code{rrace3()} returns a rank statistic corresponding
   to finishing order for a Plackett-Luce race.  The output can be
   passed to \code{ordervec2supp3()}.

   \item Function \code{rracehyper3()} returns a more complicated
   \code{hyper3} object corresponding to repeated races.

   \item Function \code{rhyper3()} returns an even more complicated
   \code{hyper3} object corresponding to repeated races and pairwise
   comparisons.
}

  Argument \code{n} generally specifies the number of distinct types of
  players.  Files \code{inst/mann_whitney_wilcoxon.Rmd} and
  \code{inst/javelin.Rmd} show some use-cases for these functions.

}
\author{Robin K. S. Hankin}
\note{
  In function \code{rracehyper3()} [and by extension \code{rhyper3()}],
  if argument \code{n} exceeds 26 and argument \code{pn} takes its
  default value of \code{NULL}, then an error will be returned because
  there are only 26 players, one for each letter \code{a}-\code{z}.
}

\seealso{\code{\link{rrank}},\code{\link{ordertable2supp}},\code{\link{ordertrans}}}
\examples{

rracehyper3()
rrace3()
rwinner3()
rhyper3()
rpair3()
ordervec2supp3(rrace3())

table(replicate(100,which(rrace3(pn=c(a=1,b=10),ps=c(a=0.9,b=0.1))=='a')))

}
