\name{pentathlon}
\alias{pentathlon}
\alias{pentathlon_table}
\alias{pentathlon_maxp}
\alias{pentathlon_ordertable}
\docType{data}
\title{Pentathlon}
\description{Results from the Men's pentathlon at the 2004 Summer Olympics}
\usage{data(pentathlon)}
\format{A \code{hyper2} object that gives a likelihood function}
\details{

Object \code{pentathlon} is a \code{hyper2} object that gives a
likelihood function for the strengths of the top seven competitors at
the Modern Men's Pentathlon, 2004 Summer Olympics.

Object \code{pentathlon_table} is an order table: a data frame with rows
being competitors, columns being disciplines, and entries being places.
Thus looking at the first row, first column we see that Moiseev placed
fifth at shooting.

These objects can be generated by running script
\code{inst/pentathlon.Rmd}, which includes some further discussion and
technical documentation and creates file \code{pentathlon.rda} which
resides in the \code{data/} directory.

}
\references{
\dQuote{Wikipedia contributors}, \emph{Modern pentathlon at the 2004
    Summer Olympics - Men's}.  Wikipedia, The Free Encyclopedia,
\url{https://en.wikipedia.org/w/index.php?title=Modern_pentathlon_at_the_2004_Summer_Olympics_\%E2\%80\%93_Men\%27s&oldid=833081611},
[Online; accessed 5-March-2020]
}
\note{Many of the competitors' names have diacritics, which I have removed.}
\seealso{\code{\link{ordertable}}}
\examples{
data(pentathlon)
pie(pentathlon_maxp)
}
