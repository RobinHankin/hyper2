\name{motoGP}
\alias{motoGP}
\alias{motoGP_2019}
\alias{motoGP_2019_table}
\docType{data}
\title{MotoGP dataset}
\description{Race results from the 2019 Grand Prix motorcycling season}
\usage{data(motoGP)}

\details{

  Object \code{motoGP_2019_table} is a dataframe of results showing
  ranks of 28 drivers (riders?) in the 2019 FIM MotoGP World
  Championship.  The format is standard, that is, can be interpreted by
  function \code{ranktable2supp()} and indeed
  \code{ranktable2supp(motoGP_2019_table)} gives the corresponding
  support function, \code{motoGP_2019}.

  File \code{inst/moto2019.txt} is the source text file; to create
  \code{motoGP_2019_table} use

  \code{a <- read.table(system.file("motoGP_2019.txt",package="hyper2"))}

  and then \code{ranktable2supp(a)} gives \code{motGP_2019}.
  
}
\references{

Wikipedia contributors. (2020, February 8). 2019 MotoGP season. In
\emph{Wikipedia, The Free Encyclopedia.} Retrieved 08:16, February 20,
2020, from \url{https://en.wikipedia.org/w/index.php?title=2019_MotoGP_season&oldid=939711064}
}
\note{
  Many drivers have names with diacritics, which have been removed from
  the dataframe.
}
\seealso{\code{\link{ranktable2supp}}}
\examples{
data(motoGP)
}
