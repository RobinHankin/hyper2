\name{increment}
\alias{increment}
\alias{decrement}
\alias{inc}
\alias{dec}
\title{Increment and decrement operators}
\description{
Syntactic sugar for incrementing and decrementing likelihood functions
}
\usage{
inc(x, val = 1)
dec(x, val = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A hyper2 object}
  \item{val}{Numeric}
}
\details{
A very frequent operation is to increment a single term in a hyper2
object.  If

\preformatted{
R> H <- hyper2(list(1,2,3,1:2,2:3),1:5)
R> pnames(H) <- letters[1:3]
}

Then we would have

\preformatted{
R> H
a * (a + b)^4 * b^2 * (b + c)^5 * c^3
}

Suppose we wish to increment the power of \code{a+b}.  We could do:

\preformatted{
H[c("a","b")] <- H[c("a","b")] + 1
}

Or use \code{magrittr} pipes:

\preformatted{
H[c("a","b")] \%<>\% `+`(1)
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

The \code{inc} and \code{dec} operators are used in
\code{inst/rowing_analysis.R}.
}
\author{Robin K. S. Hankin}
\examples{
data(chess)

## Now suppose Topalov beats Anand. To  incoroprate this into the LF:
chess["Topalov"] \%<>\% inc
chess[c("Topalov","Anand")] \%<>\% dec
chess
}