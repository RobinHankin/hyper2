\name{formula1}
\alias{formula1}
\alias{formula_1}
\alias{formula_one}
\alias{formula1_points_systems}
\alias{F1}
\alias{F1_2014}
\alias{F1_table_2016}
\alias{F1_table_2017}
\alias{F1_table_2018}
\alias{F1_table_2019}
\alias{formula1_table_2017}
\alias{F1_points_2017}
\alias{formula1_points_2017}
\alias{formula1_2017_table}
\alias{F1_2015}
\alias{F1_2016}
\alias{F1_2017}
\alias{F1_2018}
\alias{F1_2019}
\docType{data}
\title{Formula 1 dataset}
\description{Race results from 2017 Formula One World Championship}
\usage{
data(formula1)
formula1_points_systems(top=11)
}
\arguments{
  \item{top}{Number of drivers to retain in
    \code{formula1_points_systems()}}
}
\format{A \code{hyper2} object that gives a likelihood function}
\details{

Object \code{formula1} is a \code{hyper2} object that gives a likelihood
function for the strengths of the competitors of the 2017 Formula One
World Championship.  Object \code{F1_table_2017} is an order table: a
data frame with rows being drivers, columns being venues, and entries
being places.  Thus looking at the first row, first column we see that
Hamilton placed second in Austria.

Object \code{F1_table_2017} is simply the first 20 columns of
\code{read.table(inst/formula1_2017.txt)} and object
\code{F1_points_2017} is column 21.  The likelihood function
\code{formula1} is \code{ordertable2supp(F1_table_2017)}.

Function \code{formula1_points_system()} gives various possible points
systems for the winner, second, third, etc, placing drivers.  

}
\references{

  \dQuote{Wikipedia contributors}, \emph{2017 Formula One World
    Championship---{Wikipedia}{,} The Free Encyclopedia}, 2018.
  \url{https://en.wikipedia.org/w/index.php?title=2017_Formula_One_World_Championship&oldid=839923210}
  [Online; accessed 14-May-2018]
  }
\seealso{\code{\link{ordertable2supp}}}
\examples{
summary(formula1)
\dontrun{#Takes too long
dotchart(maxp(formula1))
}

}
