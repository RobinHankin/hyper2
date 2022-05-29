\name{Ops.hyper3}
\alias{Ops.hyper3}
\alias{hyper3_add}
\alias{hyper3_prod}
\alias{hyper3_sum_numeric}
\alias{hyper3_add}
\alias{sum.hyper3}
\alias{hyper3_equal}
\title{Arithmetic Ops Group Methods for hyper3 objects}
\description{

  Allows arithmetic operators \dQuote{\code{+}}, \dQuote{\code{*}} and
  comparison operators \dQuote{\code{==}} and \dQuote{\code{!=}}, to be
  used for \code{hyper3} objects.

  Specifically, \code{H1 + H2} implements addition of two log-likelihood
  functions, corresponding to incorporation of additional independent
  observational data; and \code{n*H1} implements \code{H1+H1+...+H1},
  corresponding to repeated independent observations of the same data.

}
\usage{
\method{Ops}{hyper3}(e1, e2 = NULL)
hyper3_add(e1,e2)
}
\arguments{
  \item{e1,e2}{Objects of class \code{hyper3}}
  \item{x,...,na.rm}{In the \code{sum()} method, objects to be summed;
    \code{na.rm} is currently ignored}
  \item{H,r}{In function \code{hyper3_sum_numeric()}, object \code{H} is
    a \code{hyper3} object and \code{r} is a length-one real vector (a
    number)}
}
\details{

Pretty much everything documented here is a straightforward translation
of the corresponding \code{hyper2} functionality.
  
}
\value{Returns a \code{hyper3} object or a Boolean.}
\author{Robin K. S. Hankin}
\examples{

H1 <- hyper3(list(c(a=1.2),c(b=1),c(a=1.2,b=1)),powers=c(3,4,-7))
H2 <- hyper3(list(c(a=1.2),c(b=1.2),c(a=2.2,b=1.2)),powers=c(2,3,-5))

H1
H2



}
\keyword{symbolmath}
