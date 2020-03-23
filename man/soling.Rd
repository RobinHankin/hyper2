\name{soling}
\alias{soling}
\alias{soling2000}
\alias{soling_table_2000}
\alias{soling_table}
\alias{soling2000_rr1}
\alias{soling2000_rr2}
\alias{soling2000_qf}
\docType{data}
\title{Sailing at the 2000 Summer Olympics - soling}
\description{Race results from the 2000 Summer Olympics: soling}
\usage{data(soling)}
\format{A \code{hyper2} object that gives a likelihood function}
\details{

The Soling three person keelboat event at the 2000 Summer Olympic games
furnishes a rich dataset.  An order table and likelihood function is
given in the package as \code{soling_table_2000} and \code{soling2000}
respectively.  Data from the round robins and the quarter final is given
in matrices \code{soling_2000_rr1}, \code{soling_2000_rr2},
\code{soling_2000_qf} respectively.

Other data from the same event is given on the wikipedia
page and is analysed in some detail in \code{inst/soling.Rmd}.

}
\references{

  Wikipedia contributors, \dQuote{Sailing at the 2000 Summer Olympics -
  Soling,} Wikipedia, The Free Encyclopedia,
  \url{https://en.wikipedia.org/w/index.php?title=Sailing_at_the_2000_Summer_Olympics_\%E2\%80\%93_Soling&oldid=945362535}
  (accessed March 23, 2020).

  }
\seealso{\code{\link{ordertable2supp}}}
\examples{
data(soling)
ordertable_to_ranktable(soling_table_2000)
dotchart(maxp(soling2000))
}

