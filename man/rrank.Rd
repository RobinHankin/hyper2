\name{rrank}
\alias{rrank}
\alias{print.rrank}
\title{Random ranks}
\description{
A function for producing ranks randomly, consistent with a specified
strength vector
}
\usage{
rrank(n = 1, p, pnames=NULL, fill = FALSE)
\method{print}{rrank}(x, ...)
}
\arguments{
  \item{n}{Number of observations}
  \item{p}{Strength vector}
  \item{pnames}{Character vector specifying names of the columns}
  \item{fill}{Boolean, with default \code{FALSE} meaning to interpret
    the elements of \code{p} as strengths, notionally summing to one;
    and \code{TRUE} meaning to augment \code{p} with a fillup value}
  \item{x, ...}{Arguments passed to the print method}
}

\value{
If \code{n=1}, return a vector; if \code{n>1} return a matrix with
\code{n} rows, each corresponding to a ranking.

If, say, the first row of \code{rrank()} is \code{c(2,5,1,3,4)}, then
competitor 2 came first, competitor 5 came second, and so on.

Note that function \code{rrank()} returns an object of class
\code{rrank}, which has its own special print method.  The column names
appear as \dQuote{\code{c1, c2, ...}} which is intended to be read
\dQuote{came first}, \dQuote{came second}, and so on.

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{ggol}}
}
\examples{
rrank(100,p=(4:1)/10)

r1 <- rrank(100,p=(4:1)/10)
r2 <- rrank(10,p=(4:1)/10)

H <- order_likelihood(r1)
maxp(H)

H <- H + order_likelihood(r2)
maxp(H)



}