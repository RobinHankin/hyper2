\name{T20}
\alias{T20}
\alias{T20_table}
\alias{T20_toss}
\docType{data}
\title{Indian Premier League T20 cricket}
  \description{
    Cricket dataset, T20 Indian Premier League  2008-2017
}
\usage{data(skating)}
\details{

  Dataframe \code{T20_table} has one row for each T20 IPL match in the
  period 2008-2017 with the exception of seven drawn matches and three
  no-result matches which were removed.  Object \code{T20} is a
  likelihood function for the strengths of the 13 teams, and
  \code{T20_toss} is a likelihood function that also includes a toss
  strength term.

  The likelihood functions are created \code{T20_table} and analysed in
  \code{inst/T20.Rmd}, which is itself based on Chandel and Hankin
  2019.
}
\references{
  \itemize{
    \item T. Chandel and R. K. S. Hankin 2019.  \dQuote{Analysing the
      impact of winning a coin toss in the Indian Premier League}.
    Auckland University of Technology.
  }
}
\examples{
dotchart(maxp(T20))
dotchart(maxp(T20_toss))
}
\keyword{datasets}

