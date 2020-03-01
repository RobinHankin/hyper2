\name{volvo}
\alias{volvo}
\alias{volvo_ocean_race}
\alias{volvo_table_2014}
\alias{volvo2014}
\docType{data}
\title{Race results from the 2014-2015 Volvo Ocean Race}
\description{Race results from the twelfth edition of the
  round-the-world Volvo Ocean Race.}
\usage{data(volvo)}
\format{A \code{hyper2} object that gives a likelihood function}
\details{

Object \code{volvo2014} is a \code{hyper2} object that gives a
likelihood function for the strengths of the competitors of the
2014-2015 Volvo Ocean Race.  Object \code{volvo_table_2014} is a data
frame with rows being teams and columns being legs.  Object
\code{volvo2014} is created from \code{inst/volvo2014.txt} by
\code{ordertable2supp(volvo_table_2014, noscore=c("DNF","DNS"))}.

}
\references{
Wikipedia contributors, 2019.
\dQuote{2014-2015 Volvo Ocean Race}.  In \emph{Wikipedia, the free encyclopedia}.
Retrieved 22:21, February 28, 2020.
\url{https://en.wikipedia.org/w/index.php?title=2014\%E2\%80\%932015_Volvo_Ocean_Race&oldid=914916131},

  }
\seealso{\code{\link{ordertable2supp}}}
\examples{
data(volvo)
dotchart(maxp(volvo2014))
}
