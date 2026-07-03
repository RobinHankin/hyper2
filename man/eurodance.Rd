\name{eurodance}
\alias{eurodance}
\alias{Eurodance}
\alias{eurodance_table}
\alias{eurodance_maxp}
\docType{data}
\title{Eurovision Dance contest dataset}
\description{Voting patterns from Eurovision Dance Contest 2008}
\usage{data(eurovision)}
\format{
A \code{hyper2} object that gives a likelihood
function.
}
\details{

Object \code{eurodance} is a \code{hyper2} object that gives a
likelihood function for the skills of the 14 competitor countries in
2008 Eurovision Dance contest.  Object \code{eurodance_table} gives the
original dataset and \code{eurodance_maxp} the evaluate of the
competitors' Plackett-Luce strengths.

The dataset is interesting because, in addition to the regular votes by
each nation, there is an Expert jury vote as well.  We may use
Plackett-Luce likelihoods to compare the performance of the Expert jury
with the national votes.

\generatedby{inst/eurodance.Rmd}{eurodance.rda}{https://robinhankin.github.io/hyper2/eurodance.html}
}
\references{
  \itemize{
    \item 
  Wikipedia contributors, \dQuote{Eurovision Song Contest
    2009---Wikipedia, The Free Encyclopedia}, 2018,
    \url{https://en.wikipedia.org/w/index.php?title=Eurovision_Song_Contest_2009&oldid=838723921}  [Online; accessed 13-May-2018].
    \item P. M. E. Altham, personal communication
  }
}
\seealso{
  \code{\link{eurodance}}

  \seegithub
}
\examples{
data(eurodance)
dotchart(eurodance_maxp)



}
