\name{constructor}
\alias{constructor}
\alias{constructor_2021}
\alias{constructor_2021_maxp}
\alias{constructor_table_2021}
\docType{data}
\title{Formula 1 dataset: the constructors' championship}
\description{Race results from 2017 Formula One constructors' Championship}
\usage{
data(constructor)
}
\format{A \code{hyper3} object that gives a likelihood function}
\details{

Object \code{constructor} is a \code{hyper2} object that gives a likelihood
function for the strengths of the competitors of the 2017 Formula One
World Championship.  Object \code{F1_table_2017} is an order table: a
data frame with rows being drivers, columns being venues, and entries
being places.  Thus looking at the first row, first column we see that
Hamilton placed second in Austria.

Object \code{F1_table_2017} is simply the first 20 columns of
\code{read.table(inst/constructor_2017.txt)} and object
\code{F1_points_2017} is column 21.  The likelihood function
\code{constructor} is \code{ordertable2supp(F1_table_2017)}.

Function \code{constructor_points_system()} gives various possible points
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
summary(constructor)
\dontrun{#Takes too long
dotchart(maxp(constructor))
}

}
