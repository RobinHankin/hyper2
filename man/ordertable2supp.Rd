\name{ordertable2supp}
\alias{ordertable2supp}
\alias{ordervec2supp}
\title{Translate  rank tables}
\description{

Wikipedia gives a nice summary in table form of Formula 1 racing results
on pages like
\url{https://en.wikipedia.org/wiki/2017_Formula_One_World_Championship}
(at \emph{World Drivers' Championship standings}) but the data format is
commonly used for many sports and function \code{ordertable2supp()}
translates such tables into a \code{hyper2} support function and also a
order table.

Both functions interpret zero to mean \dQuote{Did not finish} (wikipedia
usually signifies DNF as a blank).

}
\usage{
ordertable2supp(x, noscore, misslast = TRUE)
ordervec2supp(d,pnames)
}
\arguments{
  \item{x}{Data frame, see details}
  \item{d}{A named numeric vector giving order; zero entries are
    interpreted as that competitor coming last (due to, e.g., not
    finishing)}
  \item{noscore}{Character vector giving the abbreviations
    for a non-finishing status such as \dQuote{did not finish}
    or \dQuote{disqualified}.  A missing argument is interpreted as 
    \code{c("Ret", "WD", "DNS", "DSQ", "DNP", "NC")}}
  \item{misslast}{Boolean, specifying whether or not to ignore the
    last column (which is typically \dQuote{points scored})}
  \item{pnames}{Optional list of names, see details}
}
\details{

  Function \code{ordertable2supp()} is intended for use on order tables
  such as found at
  \url{https://en.wikipedia.org/wiki/2019_Moto3_season}.  This is a
  common format, used for Formula 1, motoGP, and other racing sports.
  Prepared text versions are available in the package in the
  \code{inst/} directory, for example \code{inst/motoGP_2019.txt}.  Use
  \code{read.table()} to create a data frame which can be interpreted by
  \code{ordertable2supp()}.

  


}
\value{Returns a \code{hyper2} object}
\author{Robin K. S. Hankin}
\seealso{\code{\link{formula1}}}
\examples{
data(formula1)
ordertable2supp(F1_table_2017)

a1 <- c(a=2,b=3,c=1,d=5,e=4) # a: 2nd, b: 3rd, c: 1st, d: 5th, e: 4th
a2 <- c(a=1,b=0,c=0,d=2,e=3) # a: 2nd, b: DNF, c: DNF, d: 2nd, e: 3rd
a3 <- c(a=1,b=3,c=2)         # a: 1st, b: 3rd, c: 2nd. NB only a,b,c competed
a4 <- c(a=1,b=3,c=2,d=0,e=0) # a: 1st, b: 3rd, c: 2nd, d,e: DNF
l <- letters[1:5]

H1 <- ordervec2supp(a1) + ordervec2supp(a2) + ordervec2supp(a3,pnames=l)
H2 <- ordervec2supp(a1) + ordervec2supp(a2) + ordervec2supp(a4)

pmax(H1)
pmax(H2)   # d,e not finishing affects their estimated strength


## Care!
u1 <- c(a=2,b=3,c=1,d=4,e=5)
u2 <- c(c=1,b=3,a=2,d=4,e=5)
## NB: u1,u2 correspond to the "same" statistic, but:

ordervec2supp(u1) == ordervec2supp(u2)  # FALSE
## (the pnames are different).  To get a more useful result:
ordervec2supp(u1,pnames=l) == ordervec2supp(u1,pnames=l) # TRUE


## Sometimes we need to add things to the 'did not finish' list:
ordertable2supp(volvo_table_2014,noscore=c("DNF","DNS")) == volvo2014
}
