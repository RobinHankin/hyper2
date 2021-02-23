\name{Ops.hyper2}
\alias{Ops.hyper2}
\alias{Ops}
\alias{hyper2_add}
\alias{hyper2_prod}
\alias{hyper2_sum_numeric}
\alias{sum.hyper2}
\title{Arithmetic Ops Group Methods for hyper2 objects}
\description{

  Allows arithmetic operators \dQuote{\code{+}}, \dQuote{\code{*}} and
  comparison operators \dQuote{\code{==}} and \dQuote{\code{!=}}, to be
  used for hyper2 objects.

  Specifically, \code{H1 + H2} implements addition of two log-likelihood
  functions, corresponding to incorporation of additional independent
  observational data; and \code{n*H1} implements \code{H1+H1+...+H1},
  corresponding to repeated observations of the same data.

  There are no unary operations for this class.

}
\usage{
\method{Ops}{hyper2}(e1, e2 = NULL)
\method{sum}{hyper2}(x,...,na.rm=FALSE)
hyper2_add(e1,e2)
hyper2_sum_numeric(H,r)
}
\arguments{
  \item{e1,e2}{Objects of class \code{hyper2}, here interpreted as
    hyperdirichlet distributions}
  \item{x,...,na.rm}{In the \code{sum()} method, objects to be summed;
    \code{na.rm} is currently ignored}
  \item{H,r}{In function \code{hyper2_sum_numeric()}, object \code{H} is
    a \code{hyper2} object and \code{r} is a length-one real vector (a
    number)}
}
\details{

  If two independent datasets have \code{hyper2} objects \code{H1} and
  \code{H2}, then the R idiom for combining these would be \code{H1+H2};
  the additive notation \dQuote{\code{+}} corresponds to addition of the
  support (or multiplication of the likelihood).  So \code{hyper2}
  objects are better thought of as support functions than likelihood
  functions; this is reflected in the print method which explicitly
  wraps the likelihood function in a \dQuote{\code{log()}}.

  Idiom \code{H1-H1} returns {H1 + (-1)*H2}, useful for investigating
  the difference between likelihood functions arising from two different
  observations, or different probability models.  An example is given in
  \code{inst/soling.Rmd}.
    
  Testing for equality is not straightforward for two implementation
  reasons.  Firstly, the object itself is stored internally as a
  \code{stl} \code{map}, which does not store keys in any particular
  order; and secondly, the \code{stl} \code{set} class is used for the
  brackets.  A set does not include information about the order of its
  elements; neither does it admit repeated elements.  See examples.

  Function \code{hyper2_sum_numeric()} is defined so that idiom like
  \code{icons["L"] + 5} works as expected.  This means that
  \code{icons["L"] <- icons["L"] + 3} and \code{icons["L"] \%<>\%inc(3)}
  work (without this, one has to type \code{icons["L"] <-
  powers(icons["L"]) + 3}, which sucks).
  
}
\value{
Returns a \code{hyper2} object or a Boolean.
}
\author{
Robin K. S. Hankin
}
\examples{

chess2 <- hyper2(list("Kasparov","Karpov",c("Kasparov","Karpov")),c(2,3,-5))

chess + chess2

maxp(chess+chess2)


}
\keyword{symbolmath}
