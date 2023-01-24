\name{fillup}
\alias{fillup}
\alias{indep}
\title{Fillup function}
\description{
  Function \code{fillup()} concatenates a vector with a \sQuote{fillup}
  value to ensure a unit sum; if given a matrix, attaches a column so
  the rowsums are 1.

  Function \code{indep()} is the inverse: it removes the final element
  of a vector, leaving only an independent set.
  
}
\usage{
fillup(x,H=NULL,total=1)
indep(x)
}
\arguments{
  \item{x}{Numeric vector}
  \item{H}{Object with \code{pnames()} attribute, typically of class
    \code{hyper2} or \code{hyper3}, used for names if supplied}
    \item{total}{Total value for probability}
}
\details{
  Usually you want the total to be one, to enforce the unit sum
  constraint.  Passing \code{total=0} constrains the sum to be
  zero.  This is useful when considering \eqn{\delta p}{dp}; see the
  example at \code{gradient.Rd}.
}
  
\author{Robin K. S. Hankin}
\seealso{\code{\link{equalp}},\code{\link{gradient}}}
\examples{
fillup(c(1/2,1/3))

indep(c(1/2,1/3,1/6))

fillup(indep(icons_maxp))  
fillup(indep(icons_maxp),icons)

}
