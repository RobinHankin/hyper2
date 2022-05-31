\name{increment}
\alias{increment}
\alias{decrement}
\alias{trial}
\alias{inc}
\alias{dec}
\title{Increment and decrement operators}
\description{
Syntactic sugar for incrementing and decrementing likelihood functions
}
\usage{
inc(H, val = 1)
dec(H, val = 1)
trial(winners,players,val=1)
}
\arguments{
  \item{H}{A hyper2 object}
  \item{winners,players}{Numeric or character vectors specifying the
    winning team and the losing team}
  \item{val}{Numeric}
}
\details{
A very frequent operation is to increment a single term in a hyper2
object.  If

\preformatted{
> H <- hyper2(list("b",c("a","b"),"c",c("b","c")),c(2,4,3,5))
> H
a * (a + b)^4 * b^2 * (b + c)^5 * c^3
}

Suppose we wish to increment the power of \code{a+b}.  We could do:

\preformatted{
H[c("a","b")] <- H[c("a","b")] + 1
}

(see the discussion of \code{hyper2_sum_numeric} at
\code{Ops.hyper2.Rd}).  Alternatively we could use \code{magrittr} pipes:

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


The convenience function \code{trial()} takes this one step further and
increments the \sQuote{winning team} and decrements the bracket
containing all players.  The winners are expected to be players.

\preformatted{
> trial(c("a","b"),c("a","b","c"))
> (a + b) * (a + b + c)^-1
}

Using \code{trial()} in this way ensures that the powers sum to zero.

The \code{inc} and \code{dec} operators and the \code{trial()}
function are used in \code{inst/kka.Rmd}.

}
\author{Robin K. S. Hankin}
\examples{
data(chess)

## Now suppose we observe an additional match, in which Topalov beats
## Anand.  To incorporate this observation into the LF:



trial("a",c("a","b"))

chess <- chess + trial("Topalov",c("Topalov","Anand"))
}
