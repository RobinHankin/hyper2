\name{rrank}
\alias{rrank}
\alias{rrank_single}
\alias{rorder_single}
\alias{print.ranktable}
\title{Random ranks}
\description{
A function for producing ranks randomly, consistent with a specified
strength vector
}
\usage{
rrank(n = 1, p, pnames=NULL, fill = FALSE, rnames=NULL)
\method{print}{ranktable}(x, ...)
rrank_single(p)
rorder_single(p)
}
\arguments{
  \item{n}{Number of observations}
  \item{p}{Strength vector}
  \item{pnames}{Character vector (\dQuote{player names}) specifying
    names of the columns}
  \item{rnames}{Character vector (\dQuote{row names} or \dQuote{race
      names}) specifying names of the rows}
  \item{fill}{Boolean, with default \code{FALSE} meaning to interpret
    the elements of \code{p} as strengths, notionally summing to one;
    and \code{TRUE} meaning to augment \code{p} with a fillup value}
  \item{x, ...}{Arguments passed to the print method}
}

\value{

If \code{n=1}, \code{rrank()} returns a vector; if \code{n>1} it returns
a matrix with \code{n} rows, each corresponding to a ranking.  The
canonical example is a race in which the probability of competitor
\eqn{i} coming first is \eqn{p_i/\sum p_j}{p_i/sum p}, where the
summation is over the competitors who have not already finished.

If, say, the first row of \code{rrank()} is \code{c(2,5,1,3,4)}, then
competitor 2 came first, competitor 5 came second, competitor 1 came
third, and so on.

Note that function \code{rrank()} returns an object of class
\code{ranktable}, which has its own special print method.  The column
names appear as \dQuote{\code{c1, c2, ...}} which is intended to be read
\dQuote{came first}, \dQuote{came second}, and so on.  The difference
between \emph{rank} and \emph{order} can be confusing.

\preformatted{
> x <- c(a=3.01, b=1.04, c=1.99, d=4.1)
> x
   a    b    c    d 
3.01 1.04 1.99 4.10 
> rank(x)
a b c d 
3 1 2 4 
> order(x)
[1] 2 3 1 4
}

In the above, \code{rank()} shows us that element \code{a} of \code{x}
(viz 3.01) is the third largest, element \code{b} (viz 1.04) is the
smallest, and so on; \code{order(x)} shows us that the smallest element
\code{x} is \code{x[2]}, the next smallest is \code{x[3]}, and so on.
Thus \code{x[order(x)] == sort(x)}, and \code{rank(x)[order(x)] ==
seq_along(x)}.  In the current context we want ranks not orders; we want
to know who came first, who came second, and so on:

\preformatted{
R> rrank(2,(4:1)/10)
     c1 c2 c3 c4
[1,]  2  3  1  4
[2,]  1  3  2  4
R> 
}

In the above, each row is a race; we have four runners and two races.
In the first race (the top row), runner number 2 came first, runner 3
came second, runner 1 came third, and so on.  In the second race (bottom
row), runner 1 came first, etc.  Taking the first race as an example:

\strong{Rank:} who came first? runner 2.  Who came second? runner 3.
Who came third? runner 1.  Who came fourth? runner 4.  Recall that the
Placket-Luce likelihood for a race in which the rank statistic was
\code{2314} (the first race) would be \eqn{
\frac{p_2}{p_2+p_3+p_1+p_4}\cdot
\frac{p_3}{p_3+p_1+p_4}\cdot
\frac{p_1}{p_1+p_4}\cdot
\frac{p_4}{p_4}}{omitted}.

\strong{Order:} where did runner 1 come?  third.  Where did runner 2
come?  first.  Where did runner 3 come?  second.  Where did runner 4
come?  fourth.  Thus the order statistic would be \code{3124}.

Function \code{rrank()} is designed for \code{rank_likelihood()}, which
needs rank data, not order data.  Vignette
\dQuote{\code{skating_analysis}} gives another discussion.

Note that function \code{rrank()} returns an object of class
\dQuote{\code{rrank}}, which has its own print method.  This can be
confusing.  Further details are given at \code{ranktable.Rd}.

Function \code{rrank_single()} is a low-level helper function:

\preformatted{
> p <- c(0.02,0.02,0.9,0.02,0.02,0.02)  # competitor 3 the strongest
> rank_single(p)
[1] 3 2 4 6 4 1
}

Above, we see from \code{p} that competitor 3 is the strongest, coming
first with 90\% probability.  And indeed the resulting rank statistic
given by \code{rorder_single()} shows competitor 3 coming first, 2
coming second, and so on.  Compare \code{rrank_single()}:

\preformatted{
> rorder_single(p)
[1] 6 3 1 4 5 2
> 
}

Above we see see from \code{rrank_single(p)} that competitor 1 came
sixth, competitor 2 came third, and competitor 3 came first (as you
might expect, as competitor 3 is the strongest).  Note that the R idiom
for \code{rorder_single()} is the same as that used in the
\CRANpkg{permutations} package for inverting a permutation: \code{o[o] <-
seq_along(o)}.

}
\author{Robin K. S. Hankin}
\note{Similar functionality is given by \code{rrace()}, documented at
  \link{rhyper3}.
}
\seealso{\code{\link{ordertrans}},\code{\link{rank_likelihood}},\code{\link{skating}},\code{\link{rhyper3}}}
\examples{

rrank_single(zipf(9))

ptrue <- (4:1)/10
names(ptrue) <- letters[1:4]
rrank(10,p=ptrue)

H <- rank_likelihood(rrank(40,p=ptrue))

## Following code commented out because they take too long:

# mH <- maxp(H)   # should be close to ptrue
# H <- H + rank_likelihood(rrank(30,mH)) # run some more races
# maxp(H)  # revised estimate with additional data


}
