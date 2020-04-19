\name{hessian}
\alias{hessian}
\alias{bordered_hessian}
\alias{hessian_bordered}
\title{Bordered Hessian matrix}
\description{
Returns the bordered Hessian matrix for a hyper2 object, typically at
the evaluate
}
\usage{
hessian(probs, H)
}
\arguments{
  \item{probs}{probability at which to evaluate the Hessian
}
  \item{H}{object of class \code{hyper2}}
}
\details{
still working on this
}
\value{
Returns a square matrix
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{gradient}}}
\examples{
hessian(indep(maxp(icons)),icons)
}
