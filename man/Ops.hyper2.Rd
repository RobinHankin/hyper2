\name{Ops.hyper2}
\alias{Ops.hyper2}
\alias{Ops}
\alias{hyper2_add}
\alias{hyper2_equality}
\alias{hyper2_prod}
\alias{sum.hyper2}
\title{Arithmetic Ops Group Methods for hyper2 objects}
\description{

  Allows arithmetic operators \dQuote{\code{+}}, \dQuote{\code{*}} and
  comparison operators \dQuote{\code{==}} and \dQuote{\code{!=}}, to be
  used for hyper2 objects.

  Specifically, \code{H1 + H2} implements addition of two log-likelihood
  functions, corresponding to incorporation of new observational data;
  and \code{n*H1} implements \code{H1+H1+...+H1}, corresponding to
  repeated observations of the same data.

  There are no unary operations for this class.
  
}
\usage{
\method{Ops}{hyper2}(e1, e2 = NULL)
\method{sum}{hyper2}(x,...,na.rm=FALSE)
hyper2_add(e1,e2)
hyper2_equality(e1,e2)
}
\arguments{
  \item{e1,e2}{Objects of class \code{hyper2}, here interpreted as
    hyperdirichlet distributions}
  \item{x,...,na.rm}{In the \code{sum()} method, objects to be summed;
    \code{na.rm} is currently ignored}
}
\details{

  Testing for equality is not straightforward for two implementation
  reasons.  Firstly, the object itself is stored internally as a
  \code{stl} \code{map}, which does not store keys in any particular
  order; and secondly, the \code{stl} \code{set} class is used for the
  brackets.  A set does not include information about the order of its
  elements; neither does it admit repeated elements.  See examples.
  
}
\value{
Returns a \code{hyper2} object or a Boolean.
}
\author{
Robin K. S. Hankin
}
\examples{

stopifnot(hyper2(list(1,1:2),1:2)==hyper2(list(1:2,1),2:1))  # stl map class
stopifnot(hyper2(list(1,1:2),1:2)==hyper2(list(1,c(1,1,2)),1:2)) # stl set class

}
\keyword{symbolmath}
