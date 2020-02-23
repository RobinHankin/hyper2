\name{ranktable2supp}
\alias{ranktable2supp}
\alias{rankvec2supp}
\title{Translate a rank table to a support function}
\description{

Wikipedia gives a nice summary in table form of Formula 1 racing results
on pages like
\url{https://en.wikipedia.org/wiki/2017_Formula_One_World_Championship}
(at \emph{World Drivers' Championship standings}) but the data format is
commonly used for many sports and function \code{ranktable2supp()}
translates such tables into a \code{hyper2} support function.

Both functions interpret zero to mean \dQuote{Did not finish} (wikipedia
usually signifies DNF as a blank).

}
\usage{
ranktable2supp(x, noscore, misslast = TRUE)
rankvec2supp(d,pnames)
}
\arguments{
  \item{x}{Data frame, see details}
  \item{d}{A named numeric vector giving ranks; zero entries are
    interpreted as that competitor coming last (due to, e.g., not
    finishing)}
  \item{noscore}{Character vector giving the abbreviations
    for a non-finishing status such as \dQuote{did not finish}
    or \dQuote{disqualified}.  A missing argument is interpreted as 
    \code{c("Ret", "WD", "DNS", "DSQ", "DNP", "NC")}}
  \item{misslast}{Boolean, specifying whether or not ignore the last
    column (which is typically \dQuote{points scored})}
  \item{pnames}{Optional list of names, see details}
}
\details{

  Function \code{ranktable2supp()} is intended for use on rank tables
  such as found at
  \url{https://en.wikipedia.org/wiki/2019_Moto3_season}.  A prepared
  text version is available at \code{inst/motoGP_2019.txt}; use
  \code{read.table()} to create a data frame.



}
\value{Returns a \code{hyper2} object}
\author{Robin K. S. Hankin}
\seealso{\code{\link{formula1}}}
\examples{
data(formula1)
ranktable2supp(F1_table_2017)

a1 <- c(a=1,b=2,c=5,d=4,e=3)
a2 <- c(a=1,b=0,c=0,d=2,e=3)

a3 <- c(a=1,b=3,c=2)         # only a,b,c competed
a4 <- c(a=1,b=3,c=2,d=0,e=0) # a,b,c,d,e competed, only a,b,c finished
l <- letters[1:5]

H1 <- rankvec2supp(a1) + rankvec2supp(a2) + rankvec2supp(a3,pnames=l)
H2 <- rankvec2supp(a1) + rankvec2supp(a2) + rankvec2supp(a4)

pmax(H1)
pmax(H2)   # d,e not finishing affects their estimated strength
}