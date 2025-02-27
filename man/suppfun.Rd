\name{suppfun}
\alias{suppfun}
\title{
Convert various datasets to support functions.
}
\description{
}
\usage{
\method{suppfun}{ordertable}(x)
\method{suppfun}{ranktable}(x)
\method{suppfun}{preftable}(x)
suppfun(x)
}
\arguments{
  \item{x}{Dataset to be converted to a support function
  }
}
\details{

Function \code{suppfun()} is intended as a consistent \code{S3} generic
approach to converting datasets to support functions.  I am gradually
going to switch out use of \code{ordertable2supp()} in favour of
\code{suppfun()} in the docs.

If given a data frame, \code{suppfun()} will not guess whether it is to
be interpreted as an ordertable or a ranktable, and return an error.

}
\value{
Generally, return a support function
}
\author{Robin K. S. Hankin}
\examples{

suppfun(pentathlon_table)
}
