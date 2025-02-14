\name{RCLF}
\alias{RCLF}
\alias{RCLF3}
\alias{RCLF3_maxp}
\alias{RCLF3_lambda_max}
\alias{Rangers}
\alias{Celtic}
\alias{Livingston}
\alias{Falkirk}
\docType{data}
\title{Dataset from four Scottish football clubs}
\description{

  These objects refer to results from football matches among four
  Scottish clubs: Rangers, Celtic, Livinston, Falkirk.

  Object \code{RCLF3} is a \code{hyper3} likelihood function for the
  table.  \code{RCLF3_lambda_max} is the MLE for lambda.  Object
  \code{RCLF3_maxp} is its evaluate at the MLE for lambda.

}
\usage{data(RCLF)}
\details{
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
