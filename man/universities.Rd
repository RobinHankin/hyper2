\name{universities}
\alias{universities}
\alias{universities_table}
\docType{data}
\title{New Zealand University ranking data}
\description{Times Higher Education World University Rankings}
\usage{data(universities)}
\format{A \code{hyper2} object that gives a likelihood function for
  ranking of NZ universities}
\details{
The data is taken directly from the THE website, specifying \dQuote{New Zealand}:

  \url{https://www.timeshighereducation.com/world-university-rankings/2020/world-ranking#!/page/0/length/25/locations/NZ/sort_by/rank/sort_order/asc/cols/stats}

Object \code{universities} is a \code{hyper2} support function and
\code{universities_table} a data frame.  These objects are created
\emph{ab initio} from \code{inst/universities.txt} by markdown file
\code{inst/universities.Rmd}.
  
}
\seealso{\code{\link{ordertable}}}
\examples{

pie(maxp(universities))

}

