\name{karate}
\alias{karate}
\alias{karate_table}
\docType{data}
\title{Karate dataset}
\description{
  Dataset from the 2018 World Karate Championships, men's 67kg.
}
\usage{data(karate)}

\details{

Object \code{karate_table} is a dataframe of results showing results
from the 2018 World Karate Championships, men's 67kg.  The format can be
interpreted by the idiom in \code{inst/karate_analysis.R}, which creates
\code{hyper2} object \code{karate}.

\code{read.table(system.file("karate2018.txt",package="hyper2"))}

}
\references{

Wikipedia contributors, (2019, October 9). 2018 F1 Karate World
Championship.  In \emph{Wikipedia, The Free Encyclopedia}.  Retrieved
00:45, February 21, 2020, from
\url{https://en.wikipedia.org/w/index.php?title=2018_F1_Karate_World_Championship&oldid=920386507}

}

\note{

Table \code{karate_table} misses uninformative matches, that is,
competitions with 0-0 results.

}
\seealso{\code{\link{zapweak}}}
\examples{
data(karate)
is_ok_hessian(karate)
}
