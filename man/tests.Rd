\name{tests}
\alias{equalp.test}
\alias{samep.test}
\alias{specificp.test}
\alias{print.equalptest}
\alias{print.hyper2test}
\alias{specificp.ne.test}
\alias{specificp.gt.test}
\alias{specificp.lt.test}
\alias{specificp.ge.test}
\alias{specificp.le.test}
\title{Hypothesis testing}
\description{Tests different nulls against a free alternative}
\usage{
equalp.test(H, ...)
samep.test(H, i, give=FALSE, ...)
specificp.test(H, i, specificp, alternative = c("two.sided","less","greater"),  ...)
specificp.ne.test(H, i, specificp, ...)
specificp.gt.test(H, i, specificp, ...)
specificp.lt.test(H, i, specificp, ...)
\method{print}{hyper2test}(x, ...)
}
\arguments{
  \item{H}{A likelihood function, an object of class \code{hyper2}}
  \item{...}{Further arguments passed by \code{equalp.test()} to
    \code{maxp()} and ignored by \code{print.hyper2test()}}
  \item{i,specificp}{Function \code{specificp.test()} tests the
    hypothesis that \eqn{p_i} is equal to \code{specificp}.  Function
    \code{samep.test()} has \code{i} an integer vector of strengths.
    The null is that \code{p[i[1]] == p[i[2]] == ... == p[i[r]]}}
  \item{alternative}{a character string specifying the alternative
    hypothesis, must be one of \code{two.sided} (default),
    \code{greater} or \code{less}.  You can specify just the initial
    letter (taken from \code{t.test.Rd})}
  \item{give}{Boolean, with \code{TRUE} meaning to return more
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

The tests return an object with class \code{hyper2test}, which has its
own print method.

\itemize{
  \item Function \code{equalp.test()} tests
  \eqn{H_0\colon p_1=p_2=\cdots=p_n=\frac{1}{n}}{omitted, see PDF}
  \item Function \code{specificp.test(H,i,p)} tests
  \eqn{H_0\colon p_i=p}{omitted}
  \item Function \code{samep.test()} tests \eqn{H_0\colon
  p_{i_1}=p_{i_2}=\cdots=p_{i_k}}{omitted}.
  \item Functions \code{specificp.ne.test(H,i,p)},
  \code{specificp.ne.test(H,i,p)}, and \code{specificp.ne.test(H,i,p)}
  are low-level helper functions that implement one- or two-sided versions
  of \code{specificp.test()} via the \code{alternative} argument,
  following \code{t.test()}
}
}

\value{
  The test functions return a list with class \code{"hyper2test"}
  containing the following components:
  \item{statistic}{the difference in support between the null and
    alternative}
  \item{p.value}{the (asymptotic) p-value for the test, based on Wilks's
    theorem} 
  \item{estimate}{the maximum likelihood estimate for \eqn{p}}
  \item{method}{a character string indicating what type of test was
    performed}
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
