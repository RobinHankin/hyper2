\name{equalp.test}
\alias{equalp.test}
\alias{specificp.test}
\alias{print.equalptest}
\title{Test for random outcomes}
\description{Tests the null of equal Plackett-Luce strengths.}
\usage{
equalp.test(H, ...)
\method{print}{equalptest}(x,...)
specificp.test(H, i, specificp, details=FALSE, ...)
}
\arguments{
  \item{H}{A likelihood function, an object of class \code{hyper2}}
  \item{...}{Further arguments passed by \code{equalp.test()} to
    \code{maxp()} and ignored by \code{print.equalptest()}}
  \item{i,specificp}{Function \code{specificp.test()} tests the
    hypothesis that \eqn{p_i} is equal to \code{specificp}}
  \item{details}{Boolean, with \code{TRUE} meaning to return the result of
    three separate \code{maxp()} calls, and \code{FALSE} meaning to return
    an object of class \code{equalp.test}}
  \item{x}{Object of class \code{equalp.test}, the result of
    \code{equalp.test()}}
}
\details{

Given an order table, one natural question to ask is whether the results
exhibit any structure or the ordering is totally random.  Under
Plackett-Luce, a random ordering is equivalent to the players all having
the same strength.  This null is tested against a free alternative.
An extended discussion is presented in vignette \dQuote{\code{hyper2}}.

Function \code{test.equalp()} returns an object with class
\code{equalptest}, which has its own print method.

}

\value{
  A list with class \code{"equalptest"} containing the following
  components:
  \item{statistic}{the difference in support between the null and
    alternative.}
  \item{p.value}{the (asymptotic) p-value for the test, based on Wilks's
    theorem} 
  \item{estimate}{the maximum likelihood estimate for \eqn{p}}
  \item{method}{a character string indicating what type of test was
    performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\seealso{
  \code{\link{maxp}}
}
\examples{

data(volvo)
equalp.test(volvo2014)
}
