\name{Print}
\alias{print.hyper2}
\alias{print}
\alias{Print}
\title{
Print methods
}
\description{
Print methods for \code{hyper2} objects
}
\usage{
\method{print}{hyper2}(x, ...)
}
\arguments{
  \item{x}{An object of class \code{hyper2}}
  \item{\dots}{Further arguments, currently ignored}
}
\value{

Returns the \code{hyper2} object it was sent, invisibly.  Used mainly
for its side-effect of printing the log-likelihood function.  In the
print method, a natural logarithm is indicated with
\dQuote{\code{log()}}---not \dQuote{\code{ln()}}---consistent with R
builtin terminology \code{base::log()}.

}
\author{Robin K. S. Hankin}
\examples{
data(chess)
chess
}