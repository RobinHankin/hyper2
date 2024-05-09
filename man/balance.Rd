\name{balance}
\alias{balance}
\title{Enforce the zero power sum condition}
\description{

Sometimes a \code{hyper2} object is unbalanced in the sense that its
powers do not sum to zero.  This is rectified by \code{balance()}, which
modifies the power of the bracket corresponding to the sum of all pnames
accordingly.

}
\usage{
balance(H)
}
\arguments{
  \item{H}{object of class \code{hyper2} or \code{hyper3}}
}
\details{

  This is just a convenience function, all it does is

  \preformatted{
    H[pnames(H)] <- 0
    H[pnames(H)] <- -sum(pnames(H))
    H
  }

Package vignette \code{zeropower} discusses the zero power sum
condition.
  
}
\value{Returns a balanced \code{hyper2} object}
\author{Robin K. S. Hankin}
\seealso{\code{\link{print.hyper2}}}
\examples{


H <- hyper2()
H["a"] <- 6
H["b"] <- 3
H[c("a","c")] <- 7
H <- balance(H)
maxp(H)
}
