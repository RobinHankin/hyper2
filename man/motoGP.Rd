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
  function \code{ordertable2supp()} if the final points column is
  removed.  The corresponding support function is \code{motoGP_2019}.

  File \code{inst/moto2019.txt} is the source text file; to create the R
  objects documented here use something like

\preformatted{
motoGP_2019_table <- read.table("motoGP_2019.txt",header=TRUE)
motoGP_2019 <- ordertable2supp(motoGP_2019_table[,1:19])
save(motoGP_2019_table,motoGP_2019,file="motoGP.rda")
}
  

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
\seealso{\code{\link{ordertable2supp}}}
\examples{
data(motoGP)
}
