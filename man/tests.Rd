\name{tests}
\alias{equalp.test}
\alias{knownp.test}
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
\description{\loadmathjax
  Tests different nulls against a free alternative
}
\usage{
equalp.test(H, ...)
knownp.test(H, p, ...)
samep.test(H, i, give=FALSE, ...)
specificp.test(H, i, specificp=1/size(H),
         alternative = c("two.sided","less","greater"),  ...)
specificp.ne.test(H, i, specificp=1/size(H), ...)
specificp.gt.test(H, i, specificp=1/size(H), delta=1e-5, ...)
specificp.lt.test(H, i, specificp=1/size(H), ...)
\method{print}{hyper2test}(x, ...)
}
\arguments{
  \item{H}{A likelihood function, an object of class \code{hyper2}}
  \item{p}{In \code{equalp.test()}, putative strength vector to be
    tested}
  \item{...}{Further arguments passed by \code{equalp.test()} to
    \code{maxp()} and ignored by \code{print.hyper2test()}}
  \item{i}{A character vector of names}
  \item{specificp}{Strength, real number between 0 and 1}
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
  \item{delta}{Small value for numerical stability}
}
\details{

  Given a \code{hyper2} likelihood function, there are a number of
  natural questions to ask about the strengths of the players; see
  Hankin 2010 (JSS) for examples.  An extended discussion is presented
  in vignette \dQuote{\code{hyper2}} and the functions documented here
  cover most of the tests used in the vignette.

  The tests return an object with class \code{hyper2test}, which has its
  own print method.

\itemize{
  \item Function \code{equalp.test(H,p)} tests the null that all
  strengths are equal to vector \code{p}.  If \code{p} is missing, it
  tests \mjeqn{H_0\colon p_1=p_2=\cdots=p_n=\frac{1}{n}}{H0:
    p1=p2=...=pn=1/n}, for example \code{equalp.test(icons)}
  \item Function \code{knownp.test()} tests the null that the strengths
  are equal to the elements of named vector \code{p}; it is a
  generalization of \code{equalp.test()}.  Example:
  \code{knownp.test(icons,zipf(6))}. 
  \item Function \code{specificp.test(H,i,p)} tests
  \mjeqn{H_0\colon p_i=p}{H0: p_i=p}, for example
  \code{specificp.test(icons,"NB",0.1)}
  \item Function \code{samep.test()} tests \mjeqn{H_0\colon
  p_{i_1}=p_{i_2}=\cdots=p_{i_k}}{omitted}, for example
  \code{samep.test(icons,c("NB","L"))} tests that \code{NB} has the same
  strength as \code{L}. 
  \item Functions \code{specificp.ne.test(H,i,p)},
  \code{specificp.gt.test(H,i,p)}, and \code{specificp.lt.test(H,i,p)}
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
\seealso{\code{\link{maxp}}}

\note{
Function \code{specificp.gt.test()} includes quite a bit of messing
about to ensure that frequently-used idiom like
\code{specificp.gt.test(icons,"NB",0)} works as expected, testing a null
of \code{p_NB=0}.  In the case of testing a strength's being zero, the
support function is often quite badly-behaved near the constraint [think
tossing a coin with probability \eqn{p} twice, observing one head and
one tail, and testing \eqn{p=0}; at the constraint, the likelihood is
zero, the support negative infinity, and the gradient of the support is
infinite].  Numerically, the code tests \code{p_NB=delta}.  Note that
similar machinations are not required in \code{specificp.lt.test()}
because a null of \code{p_NB=1} is unrealistic.

Function \code{samep.test()} does not have access to gradient
information so it is slow, inaccurate, and may fail completely for
high-dimensional datasets.  If \code{any(i==n)}, this constrains the
fillup value; this makes no difference mathematically but the function
idiom is involved.

}

\examples{

equalp.test(chess)

# samep.test(icons,c("NB","L"))
# knownp.test(icons,zipf(icons))

}
