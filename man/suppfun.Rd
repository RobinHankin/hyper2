\name{suppfun}
\alias{suppfun}
\alias{suppfun.ordertable}
\alias{suppfun.ranktable}
\alias{suppfun.character}
\alias{suppfun.numeric}
\title{
Convert various datasets to support functions.
}
\description{

Function \code{suppfun()} is an S3 generic that returns a support
function, dispatching on the class of its primary argument.  

  
}
\usage{
\method{suppfun}{ordertable}(x, ...)
\method{suppfun}{ranktable}(x, times, ...)
\method{suppfun}{character}(x, nonfinishers, ...)
suppfun(x, ...)
}
\arguments{
  \item{x}{Dataset to be converted to a support function}
  \item{times}{Vector corresponding to rows of \code{x} giving how many
    times that observation was obtained (cf \code{preflib} datasets);
    recycled if necessary}
  \item{nonfinishers}{Character vector of competitors who did not
    finish, passed to \code{rankvec_likelihood()}}
  \item{...}{Further arguments}
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
suppfun(rrank(10, p = (7:1)/28))
suppfun(letters)
suppfun(c(c = 2, b = 1, a = 3))



}
