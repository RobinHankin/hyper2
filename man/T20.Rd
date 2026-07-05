\name{T20}
\alias{T20}
\alias{T20_table}
\alias{T20_toss}
\alias{T20_maxp}
\alias{T20_toss_maxp}
\docType{data}
\title{Indian Premier League T20 cricket}
  \description{
    Cricket dataset, T20 Indian Premier League  2008-2017
}
\usage{data(T20)}
\details{

  Dataframe \code{T20_table} has one row for each T20 \acronym{IPL}
  match in the period 2008-2017 with the exception of seven drawn
  matches and three no-result matches which were removed.  Object
  \code{T20} is a likelihood function for the strengths of the 13 teams,
  and \code{T20_toss} is a likelihood function that also includes a toss
  strength term.

  \generatedby{inst/T20.Rmd}{T20.rda}{https://robinhankin.github.io/hyper2/T20.html}

}
\references{
  \itemize{
    \item T. Chandel and R. K. S. Hankin 2019.  \dQuote{Analysing the
      impact of winning a coin toss in the Indian Premier League}.
    Auckland University of Technology.
  }
}
\seealso{\seegithub}
\examples{
summary(T20)
dotchart(T20_maxp)

}
\keyword{datasets}

