\name{equalp.test}
\alias{equalp.test}
\alias{specificp.test}
\alias{samep.test}
\alias{print.equalptest}
\alias{print.hyper2test}
\title{Hypothesis testing}
\description{Tests different nulls against a free alternative}
\usage{
equalp.test(H, ...)
\method{print}{hyper2test}(x, ...)
specificp.test(H, i, specificp, details=FALSE, ...)
samep.test(H, i, give=FALSE, ...)
}
\arguments{
  \item{H}{A likelihood function, an object of class \code{hyper2}}
  \item{...}{Further arguments passed by \code{equalp.test()} to
    \code{maxp()} and ignored by \code{print.hyper2test()}}
  \item{i,specificp}{Function \code{specificp.test()} tests the
    hypothesis that \eqn{p_i} is equal to \code{specificp}.  Function
    \code{samep.test()} has \code{i} an integer vector of strengths.
    The null is that \code{p[i[1]] == p[i[2]] == ... == p[i[r]]}}
  \item{give, details}{Boolean, with \code{TRUE} meaning to return more
    detailed debugging information, and default \code{FALSE} meaning to
    return a more user-friendly object of class \code{equalp.test},
    which has its own print method}
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
\code{hyper2test}, which has its own print method.

}

\value{
  The test functions return a list with class \code{"hyper2test"}
  containing the following components:
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

specificp.test(volvo2014,1,0.1)
}
