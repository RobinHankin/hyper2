\name{RCLF}
\alias{RCLF}
\alias{RCLF3}
\alias{RCLF3_maxp}
\alias{RCLF3_lambda_max}
\alias{RCLF3_table}
\alias{Rangers}
\alias{Celtic}
\alias{Livingston}
\alias{Falkirk}
\docType{data}
\title{Dataset from four Scottish football clubs}
\description{
Datasets for match results for four Scottish clubs: Rangers, Celtic,
  Livingston, Falkirk.
  }

\usage{data(RCLF)}
\details{
  These objects refer to results from football matches among four
  Scottish clubs: Rangers, Celtic, Livingston, Falkirk.  A detailed
  analysis is presented in \code{inst/home_advantage.Rmd}, which creates
  the objects documented here from scratch.

  \enumerate{
    \item  Object \code{RCLF3_table} is a table of results for the four
    clubs: home wins, draws, and away wins.
    \code{RCLF3_lambda_max} for the maximum likelihood estimate for
    \eqn{\lambda}.
    \item Object \code{RCLF3} is a \code{hyper3} likelihood function for
    the table of won games only [that is, excluding draws], using
    \code{RCLF3_lambda_max} for the value of \eqn{\lambda}.
    \item Object \code{RCLF3_maxp} is its evaluate at the MLE for
    lambda}
The objects are created by \code{inst/home_advantage.Rmd}.
}
\source{
  Data obtained from \code{www.worldfootball.net}
}
\author{Robin K. S. Hankin}
\examples{
data(RCLF)
RCLF3_table
RCLF3_maxp
RCLF3_lambda_max

loglik(RCLF3_maxp,RCLF3)
equalp.test(RCLF3)
}
\keyword{datasets}
