\name{universities}
\alias{universities}
\alias{universities_table}
\alias{universities_maxp}
\docType{data}
\title{New Zealand University ranking data}
\description{Times Higher Education World University Rankings}
\usage{data(universities)}
\format{A \code{hyper2} object that gives a likelihood function for
  ranking of \acronym{NZ} universities}
\details{
The data is taken directly from the \acronym{THE} website, specifying \dQuote{New Zealand}:

  \url{https://www.timeshighereducation.com/world-university-rankings/2020/world-ranking#!/page/0/length/25/locations/NZ/sort_by/rank/sort_order/asc/cols/stats}

Object \code{universities} is a \code{hyper2} support function and
\code{universities_table} a data frame.  

\generatedby{inst/universities.Rmd}{universities.rda}{https://robinhankin.github.io/hyper2/universities.html}
  
}
\seealso{\code{\link{ordertable}}}
\examples{

summary(universities)

psubs(universities,c("AUT","UoA"),c("University of Auckland","Auckland University of Technology"))

pie(universities_maxp)

}

