\name{karate}
\alias{karate}
\alias{karate_table}
\alias{karate_maxp}
\alias{karate_zermelo}
\docType{data}
\title{Karate dataset}
\description{
  Dataset from the 2018 World Karate Championships, men's 67kg.  It is
  an example of a dataset with too many degrees of freedom to be
  analysed easily by the package.
}
\usage{data(karate)}

\details{

Object \code{karate_table} is a dataframe of results showing results
from the 2018 World Karate Championships, men's 67kg; \code{karate} is
the associated likelihood function.  There are two maximum likelihood
estimates given; \code{karate_maxp}, the evaluate as returned by
\code{maxp()}, and \code{karate_maxp}, returned by \code{zermelo()} [the
value given by \code{maxp()} itself is less likely].

\generatedby{inst/karate.Rmd}{karate.rda}{https://robinhankin.github.io/hyper2/karate.html}


}
\references{
\url{https://en.wikipedia.org/wiki/2018_World_Karate_Championships}
}
\note{
Table \code{karate_table} misses uninformative matches, that is,
competitions with 0-0 results.

}
\seealso{
  \code{\link{zapweak}}

  \seegithub
}
\examples{
summary(karate)
}
