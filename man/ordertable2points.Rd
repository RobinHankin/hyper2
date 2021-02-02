\name{ordertable2points}
\alias{ordertable2points}
\title{Calculate points from an order table}
\description{

  Given an order table and a schedule of points, calculate the points
  awarded to each competitor.
}

\usage{
ordertable2points(o, points,totals=TRUE)
}
\arguments{
  \item{o}{Order table}
  \item{points}{A numeric vector indicating number of points awarded for
    first, second, third, etc placing}
  \item{totals}{Boolean, with default \code{TRUE} meaning to return the
    points for each player (row) and \code{FALSE} meaning to return the
    entire table but with orders replaced with points scored}
}
\value{Returns either an order table or a named numeric vector}
\author{Robin K. S. Hankin}
\seealso{\code{\link{ordertable}}}
\examples{

points <- c(25, 18, 15, 12, 10, 8, 6, 4, 2, 1, 0, 0)
o <- as.ordertable(F1_table_2017)
ordertable2points(o,points)

ordertable2points(ranktable_to_ordertable(rrank(9,volvo_maxp)),1)

}
