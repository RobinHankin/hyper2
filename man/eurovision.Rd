\name{eurovision}
\alias{euro}
\alias{euro2009}
\alias{eurovision}
\alias{Eurovision}
\alias{Eurovision2009}
\alias{eurovision2009}
\alias{eurovision2009_votingtable}
\alias{Eurovision_song_contest}
\alias{eurovision_table}
\alias{eurovision_maxp}
\docType{data}
\title{Eurovision Song contest dataset}
\description{Voting patterns from Eurovision 2009}
\usage{data(eurovision)}
\format{
A \code{hyper2} object that gives a likelihood
function.
}
\details{

Object \code{eurovision} is a \code{hyper2} object that gives a
likelihood function for the skills of the 18 competitor countries in
semi-final 1 of the 2009 Eurovision Song contest.  It is created by the
self-contained code in \code{inst/eurovision.Rmd}, which is heavily
documented.

The motivation for choosing this particular dataset is that Pat Altham
(Statistical Laboratory, Cambridge) considered it with a view to
discover similarities between voters.  In the current analysis, the
likelihood function \code{eurovision} assumes their independence.

Object \code{eurovisiontable} is taken from the wikipedia
page; the source is at \code{inst/eurovision.txt}.

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
dotchart(eurovision_maxp)



}
