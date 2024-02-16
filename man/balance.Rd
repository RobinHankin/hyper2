\name{balance}
\alias{balance}
\title{Balance the powers of a \code{hyper2} object}
\description{
Sometimes a \code{hyper2} object is unbalanced in the sense that its
powers do not sum to zero.  This is rectified by \code{balance()}. 
}
\usage{
balance(H)
}
\arguments{
  \item{H}{object of class \code{hyper2}
}
}
\value{
Returns a balanced \code{hyper2} object
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{print.hyper2}}}
\examples{


H <- hyper2()
H["a"] <- 6
H["b"] <- 3
H <- balance(H)
maxp(H)
}