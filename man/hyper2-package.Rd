\name{hyper2-package}
\alias{hyper2-package}
\alias{hyperdirichlet2}
\docType{package}
\title{
  A generalization of the Dirichlet distribution
}
\description{
  A generalization of the Dirichlet distribution, using a more
  computationally efficient method than the \pkg{hyperdirichlet}
  package.  The software is designed for the analysis of order
  statistics. 
}
\details{
The \pkg{hyper2} package is an update of the \pkg{hyperdirichlet}
package which is more efficient and can deal with larger datasets.  The
software is based on the \code{STL} \code{map} class of \code{C++}.
}
\author{
Robin K. S. Hankin, <hankin.robin@gmail.com>
}
\references{
The vignette is  currently under review at JSS
}
\keyword{ package }

\examples{
data(chess)
maxp(chess)  # MLE for players' strengths

H <- hyper2(pnames=letters[1:5])

H <- H + order_likelihood(rrank(100,5:1)) # probs = 5/15,4/15,...,1/15
maxp(H)  # should be close to (5:1)/15

}
