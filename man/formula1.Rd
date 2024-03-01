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
(Drivers') World Championship.  Object \code{F1_table_2017} is an order table: a
data frame with rows being drivers, columns being venues, and entries
being places.  Thus looking at the first row, first column we see that
Hamilton placed second in Austria.

The package uses files like \code{inst/formula1_2017.txt} as primary
sources.  These are generally copied from wikipedia, converted into
tab-separated clean seven bit ascii, and tidied up a little.  I have
removed diacritics from names, so we see \dQuote{\code{Raikkonen}},
\dQuote{\code{Perez}}, etc.  Also where distinct drivers with the same
surname compete, I have indicated this, e.g. \code{schumacher_R} is Ralf
Schumacher, \code{schumacher_M} is Michael, and \code{schumacher_Mick}
is Mick; the underscore device means that quoting should not be needed
in \R idiom.  I have not been entirely consistent here, with Bruno Senna
appearing as \dQuote{\code{Senna_B}} and Nelson Piquet Junior appearing
as \dQuote{\code{PiquetJ}} [on the grounds that in these cases the
fathers, being more eminent, should be the primary eponym] although this might
change in the future.

Object \code{F1_table_2017} is simply the first 20 columns of
\code{read.table(inst/formula1_2017.txt)} and object
\code{F1_points_2017} is column 21.  The likelihood function
\code{formula1} is \code{ordertable2supp(F1_table_2017)}.  The datasets
in the package are derived from text files in the \code{inst/} directory
(e.g. \code{formula1_2017.txt}) by script file
\code{inst/f1points_Omaker.R}.  Executing this script creates files like
\code{formula1_results_2017.rda}.

The text files can be converted directly into \code{ranktable} objects
and support functions as follows:

\preformatted{
a <- read.table("formula1_2022.txt",header=TRUE)
a <- a[,seq_len(ncol(a)-1)]  # strips out the points column
wikitable_to_ranktable(a)
ordertable2supp(a)
}





Function \code{formula1_points_system()} gives various possible points
systems for the winner, second, third, etc, placing drivers.  

The constructors' championship is discussed at \code{constructor.Rd}.

There is a large amount of documentation in the \code{inst/} directory
in the form of Rmd files.




}
\references{

  \dQuote{Wikipedia contributors}, \emph{2017 Formula One World
    Championship---Wikipedia, The Free Encyclopedia}, 2018.
  \url{https://en.wikipedia.org/w/index.php?title=2017_Formula_One_World_Championship&oldid=839923210}
  [Online; accessed 14-May-2018]
  }
\seealso{\code{\link{ordertable2supp}},\code{\link{constructor}}}
\examples{
summary(formula1)
\dontrun{#Takes too long
dotchart(maxp(formula1))
}

}
