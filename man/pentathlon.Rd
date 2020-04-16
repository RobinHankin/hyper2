\name{pentathlon}
\alias{pentathlon}
\alias{pentathlon_ordertable}
\docType{data}
\title{Formula 1 dataset}
\description{Results from the Men's pentathlon at the 2004 Summer Olympics}
\usage{data(pentathlon)}
\format{A \code{hyper2} object that gives a likelihood function}
\details{

Object \code{pentathlon} is a \code{hyper2} object that gives a
likelihood function for the strengths of the top seven competitors at
the Modern Men's Pentathlon, 2004 Summer Olympics.

Object \code{pentathlon_ordertable} is an order table: a data frame with
rows being competitors, columns being disciplines, and entries being
places.  Thus looking at the first row, first column we see that
Moiseev placed fifth at shooting.

Object \code{pentathlon_ordertable} is simply
\code{read.table(inst/pentathlon.txt)} and \code{pentathlon} is
\code{ordertable2supp(pentathlon_ordertable)}.

}
\references{
\dQuote{Wikipedia contributors}, \emph{Modern pentathlon at the 2004
    Summer Olympics - Men's}.  {Wikipedia}{,} The Free Encyclopedia,
\url{https://en.wikipedia.org/w/index.php?title=Modern_pentathlon_at_the_2004_Summer_Olympics_\%E2\%80\%93_Men\%27s&oldid=833081611},
[Online; accessed 5-March-2020]
}
\note{Many of the competitors' names have diacritics, which I have removed.}
\seealso{\code{\link{ordertable}}}
\examples{
data(pentathlon)
dotchart(maxp(pentathlon,n=2))  #n=2 specified here for speed
}
