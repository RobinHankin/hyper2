\name{ranktable2supp}
\alias{ranktable2supp}
\title{Translate a rank table to a support function}
\description{

Wikipedia gives a nice summary in table form of Formula 1 racing results
on pages like
\url{https://en.wikipedia.org/wiki/2017_Formula_One_World_Championship}
(at \emph{World Drivers' Championship standings}) but the data format is
commonly used for many sports and package function
\code{ranktable2supp()} translates such tables into a \code{hyper2}
support function.

The function interprets zero to mean \dQuote{Did not finish} (wikipedia
usually signifies DNF as a blank).

}
\usage{
ranktable2supp(x, noscore, misslast = TRUE)
}
\arguments{
  \item{x}{Data frame, see details}
  \item{noscore}{Character vector giving the abbreviations
    for a non-finishing status such as \dQuote{did not finish}
    or \dQuote{disqualified}.  A missing argument is interpreted as 
    \code{c("Ret", "WD", "DNS", "DSQ", "DNP", "NC")}}
  \item{misslast}{Boolean, specifying whether or not ignore the last
    column (which is typically \dQuote{points scored})}
}
\details{
Function \code{ranktable2supp()} is intended for use on rank tables
such as found at \url{https://en.wikipedia.org/wiki/2019_Moto3_season}. 
}
\value{Returns a \code{hyper2} object}
\author{Robin K. S. Hankin}
\seealso{\code{\link{formula1}}}
\examples{
data(formula1)
ranktable2supp(F1_table_2017)
}