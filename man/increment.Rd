\name{increment}
\alias{increment}
\alias{decrement}
\alias{inc}
\alias{dec}
\title{Increment and decrement operators}
\description{
  Syntactic sugar for incrementing and decrementing likelihood functions.
  Useful for constructs such as \code{H[c("a", "b")] \%<>\% inc()}.
}
\usage{
inc(H, val = 1)
dec(H, val = 1)
}
\arguments{
  \item{H}{A hyper2 object}
  \item{val}{Numeric}
}
\details{

A very frequent operation is to increment a single term in a
\code{hyper2} object.  If

\preformatted{
>  H <- hyper2(list("a", c("a","b"), "c", c("a","b","c")), c(1:3,-6))
>  H
log( a * (a + b + c)^-6 * b^2 * c^3)
}

Suppose we wish to increment the power of \code{a+b}.  We could do:

\preformatted{
H[c("a","b")] <- H[c("a","b")] + 1
}

(see the discussion of \code{hyper2_sum_numeric} at
\code{Ops.hyper2.Rd}; also vignette \code{zeropower}).  Alternatively we
could use \code{magrittr} pipes:

\preformatted{
H[c("a","b")] \%<>\% add(1)
}

But \code{inc} and \code{dec} furnish convenient idiom to accomplish the
same thing:

\preformatted{
H[c("a","b")] \%<>\% inc
}

Functions \code{inc} and \code{dec} default to adding or subtracting 1,
but other values can  be supplied:

\preformatted{
H[c("a","b")] \%<>\% inc(3)
}

Or even 

\preformatted{
H[c("a","b")] \%<>\% inc(H["a"])
}

The deprecated convenience function \code{trial()} uses this idiom; see
\code{pick.Rd}.  Using \code{trial()} in this way ensures that the
powers sum to zero (unlike \code{inc()} and \code{dec()}.

The \code{inc} and \code{dec} operators and the \code{pick()} function
are used in \code{inst/kka.Rmd}.

Frankly \code{inc()} and \code{dec()} don't do anything that
\code{magrittr:add()} and \code{magrittr:subtract()} don't (except have
a default value of 1 (which is surprisingly useful)).

}
\author{Robin K. S. Hankin}
\examples{

(H <- hyper2(list("a",c("a","b"),c("a","b","c")),c(1,1,-2)))
H["a"] %<>% inc
H[c("a","x")] %<>% dec
H
cd

}
