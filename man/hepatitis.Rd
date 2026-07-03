\name{hepatitis}
\alias{hepatitis}
\alias{hepatitis_table}
\alias{hepatitis_maxp}
\alias{hepatitis_count}
\docType{data}
\title{Hepatitis dataset discussed by Agresti}
\description{A dataset considered by Agresti}
\usage{data(hepatitis)}
\format{A \code{hyper2} object that gives a likelihood function}
\details{

Object \code{hepatitis_table} is drawn from Agresti, table 12.16, page
533.  Object \code{hepatitis} is a likelihood function of class
\code{lsl} and \code{hepatitis_maxp} a pre-calculated evaluate.

\generatedby{inst/hepatitis.Rmd}{hepatitis.rda}{https://robinhankin.github.io/hyper2/hepatitis.html}

}
\references{
A. Agresti, 2002.  "Categorical data analysis".  John Wiley and
Sons. Table 13.1, p542.
 }
 \seealso{
   \code{\link{race3}},
   \code{\link{hepatitis}}

   \seegithub
 }
\examples{
pie(hepatitis_maxp)
}

