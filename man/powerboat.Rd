\name{powerboat}
\alias{powerboat
\alias{powerboat2018}
\alias{powerboat_2018}
\alias{powerboat2018_table}
\docType{data}
\title{Powerboat dataset}
\description{Race results from the 2018 F1 Powerboat World Championship}
\usage{data(powerboat)}

\details{

  Object \code{powerboat2018_table} is a dataframe of results showing
  ranks of 21 drivers in the 2018 F1 Powerboat World Championship.  The
  format is standard, that is, can be interpreted by function
  \code{ranktable2supp()} and indeed
  \code{ranktable2supp(powerboat2018_table)} gives the corresponding
  support function, \code{powerboat2018}.

  File \code{inst/powerboat2018.txt} is the source text file; to create
  \code{powerboat2018_table} use

  \code{read.table(system.file("powerboat2019.txt",package="hyper2"),header=TRUE)}
  
}
\references{

Wikipedia contributors. (2019, October 9). 2018 F1 Powerboat World
Championship. In \emph{Wikipedia, The Free Encyclopedia}.  Retrieved
00:45, February 21, 2020, from
\url{https://en.wikipedia.org/w/index.php?title=2018_F1_Powerboat_World_Championship&oldid=920386507}

}

\note{
  Many drivers have names with diacritics, which have been removed from
  the dataframe.
}
\seealso{\code{\link{ranktable2supp}}}
\examples{
data(powerboat)
jj <-
  read.table(system.file("powerboat2019.txt",package="hyper2"),header=TRUE)
identical(jj,powerboat2019_table)  # should be TRUE
}
