\name{ordertable2supp}
\alias{ordertable2supp}
\alias{ordervec2supp}
\alias{ordervec2supp}
\alias{order_obs}
\title{Translate order tables to support functions}
\description{\loadmathjax

Wikipedia gives a nice summary in table form of Formula 1 racing results
on pages like
\url{https://en.wikipedia.org/wiki/2017_Formula_One_World_Championship}
(at \emph{World Drivers' Championship standings}) but the data format is
commonly used for many sports [see \code{ordertable.Rd}] and function
\code{ordertable2supp()} translates such tables into a \code{hyper2}
support function and also a order table.

Both functions interpret zero to mean \dQuote{Did not finish} (wikipedia
usually signifies DNF as a blank).

}
\usage{
ordertable2supp(x, noscore, incomplete=TRUE)
ordervec2supp(d)
}
\arguments{
  \item{x}{Data frame, see details}
  \item{d}{A named numeric vector giving order; zero entries are
    interpreted as that competitor coming last (due to, e.g., not
    finishing)}
  \item{incomplete}{Boolean, with \code{FALSE} meaning to insist that
    each rank \eqn{1,2,...,n} is present [zero meaning did not place]
    and default \code{TRUE} allowing for gaps.  See examples.}
  \item{noscore}{Character vector giving the abbreviations
    for a non-finishing status such as \dQuote{did not finish}
    or \dQuote{disqualified}.  A missing argument is interpreted as 
    \code{c("Ret", "WD", "DNS", "DSQ", "DNP", "NC")}}
}
\details{

Function \code{ordertable2supp()} is intended for use on order tables
such as found at \url{https://en.wikipedia.org/wiki/2019_Moto3_season}.
This is a common format, used for Formula 1, motoGP, and other racing
sports.  Prepared text versions are available in the package in the
\code{inst/} directory, for example \code{inst/motoGP_2019.txt}.  Use
\code{read.table()} to create a data frame which can be interpreted by
\code{ordertable2supp()}.

Function \code{ordervec2supp()} takes an order vector \code{d} and
returns the corresponding Plackett-Luce loglikelihood function as a
\code{hyper2} object.  It requires a named vector; names of the elements
are interpreted as names of the players.  Use argument \code{pnames} to
supply the players' names (see the examples).

\preformatted{
> x <- c(b=2,c=3,a=1,d=4,e=5) # a: 1st, b: 2nd, c: 3rd etc
> ordervec2supp(x)
log( a * (a + b + c + d + e)^-1 * (a + b + d + e)^-1 * b * (b + d +
e)^-1 * c * (d + e)^-1 * e)
}

\mjdeqn{
\frac{a}{a+b+c+d+e}\cdot
\frac{b}{b+c+d+e}\cdot
\frac{c}{c+d+e}\cdot
\frac{d}{d+e}\cdot
\frac{e}{e}
}{
  omitted
}

Note carefully the difference between \code{ordervec2supp()} and
\code{rankvec_likelihood()}, which takes a character vector:

\preformatted{
>  names(sort(x))
[1] "a" "b" "c" "d" "e"
> rankvec_likelihood(names(sort(x)))
log( a * (a + b + c + d + e)^-1 * b * (b + c + d + e)^-1 * c * (c + d +
e)^-1 * d * (d + e)^-1)
> rankvec_likelihood(names(sort(x))) == ordervec2supp(x)
[1] TRUE
> 
}

Function \code{order_obs()} was used in the integer-indexed paradigm but
is obsolete in the name paradigm.
}

\value{Returns a \code{hyper2} object}
\author{Robin K. S. Hankin}
\seealso{\code{\link{ordertable}}}
\examples{
data(formula1)
ordertable2supp(F1_table_2017)

a1 <- c(a=2,b=3,c=1,d=5,e=4) # a: 2nd, b: 3rd, c: 1st, d: 5th, e: 4th
a2 <- c(a=1,b=0,c=0,d=2,e=3) # a: 2nd, b: DNF, c: DNF, d: 2nd, e: 3rd
a3 <- c(a=1,b=3,c=2)         # a: 1st, b: 3rd, c: 2nd. NB only a,b,c competed
a4 <- c(a=1,b=3,c=2,d=0,e=0) # a: 1st, b: 3rd, c: 2nd, d,e: DNF


## ordervec2supp() may be added [if the observations are independent]:

H1 <- ordervec2supp(a1) + ordervec2supp(a2) + ordervec2supp(a3)
H2 <- ordervec2supp(a1) + ordervec2supp(a2) + ordervec2supp(a4)

## Thus H1 and H2 are identical except for the third race.  In H1, 'd'
## and 'e' did not compete, but in H2, 'd' and 'e' did not finish (and
## notionally came last):

pmax(H1)
pmax(H2)   # d,e not finishing affects their estimated strength

}
