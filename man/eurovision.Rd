\name{eurovision}
\alias{euro}
\alias{euro2009}
\alias{eurovision}
\alias{Eurovision}
\alias{Eurovision2009}
\alias{eurovision2009}
\alias{eurovision2009_votingtable}
\alias{Eurovision_song_contest}
\docType{data}
\title{Eurovision Song contest dataset}
\description{Voting patterns from Eurovision 2009}
\usage{data(euro)}
\format{
A \code{hyper2} object that gives a likelihood
function.
}
\details{

Object \code{eurovision2009} is a \code{hyper2} object that gives a
likelihood function for the skills of the 18 competitor countries in
semi-final 1 of the 2009 Eurovision Song contest.  It is created by the
self-contained code in \code{inst/eurovision.Rmd}, which is heavily
documented.

The motivation for choosing this particular dataset is that Pat Altham
(Statistical Laboratory, Cambridge) considered it with a view to
discover similarities between voters.  In the current analysis, the
likelihood function \code{eurovision2009} assumes their independence.

Object \code{eurovision2009_votingtable} is taken from the wikipedia
page; the source is at \code{inst/eurovision2009.txt}.

}
\references{
  \itemize{
    \item 
  Wikipedia contributors, \dQuote{Eurovision Song Contest
    2009---{Wikipedia}{,} The Free Encyclopedia}, 2018,
    \url{https://en.wikipedia.org/w/index.php?title=Eurovision_Song_Contest_2009&oldid=838723921}  [Online; accessed 13-May-2018].
    \item P. M. E. Altham, personal communication
  }
}
\examples{
data(eurovision)

dotchart(maxp(eurovision2009,n=2)) # n=2 for speed

x1 <- loglik(indep(maxp(eurovision2009,n=2)),eurovision2009)  
x2 <- loglik(indep(equalp(eurovision2009)),eurovision2009)

## if the competitors have equal strength, the asymptotic distribution
##   of 2*(x1-x2) is chi-square with 17 degrees of freedom:

pchisq(2*(x1-x2),df=size(eurovision2009)-1,lower.tail=FALSE)
}
