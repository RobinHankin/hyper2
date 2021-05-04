\name{consistency}
\alias{consistency}
\alias{consistencyplot}
\title{Consistency check for hyper2 objects}
\description{
Given a \code{hyper2} object, calculate the maximum likelihood point in
two ways and plot one against the other to check for consistency.
}
\usage{
consistency(H, plot=TRUE, ...)
}
\arguments{
  \item{H}{A \code{hyper2} object}
  \item{plot}{If \code{TRUE} (default), plot a comparison and return a
    matrix invisibly, and if \code{FALSE} return the matrix.  Modelled
    on argument \code{plot} of \code{hist}}
  \item{...}{Further arguments, passed to \code{points()}}
}

\details{

  Given a \code{hyper2} object, calculate the maximum likelihood
  estimate of the players' strengths using \code{maxp()}; then reverse
  the \code{pnames} attribute and calculate the players' strengths
  again.  These two estimates should be identical but small differences
  highlight numerical problems.  Typically, the differences are small if
  there are fewer than about 25 players.

  Reversing the \code{pnames()} is cosmetic in theory but is a
  non-trivial operation: for example, it changes the identity of the
  fillup from the last player to the first.
    
}

\value{Returns a named three-row matrix with first row being the direct
  evaluate, second row being the reverse of the reversed evaluate, and
  the third being the difference
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{ordertrans}}}
\examples{

consistency(icons)

x <- icons
y <- icons
pnames(y) <- rev(pnames(y))
gradient(x,indep(equalp(x)))
gradient(y,indep(equalp(y)))

}


