\name{masterchef}
\Rdversion{1.1}
\alias{masterchef}
\alias{MasterChef}
\alias{masterchef_maxp}
\alias{masterchef_constrained_maxp}
\docType{data}
\title{
Masterchef series 6
}
\description{
Data from Australian Masterchef Series 6
}
\usage{data(masterchef)}

\format{

Object \code{masterchef} is a list of \code{hyper2} objects;
\code{masterchef_pmax} and \code{masterchef_constrained_pmax} are named
vectors with unit sum.
  
}
\details{

The object is created using the code in \code{inst/masterchef.Rmd},
which is heavily documented.  Not all the information available is
included in the likelihood function as some of the early rounds result
in an unmanageably large list.  Inclusion is controlled by Boolean
vector \code{doo}.
 
The definitive source is the coloured table on the wiki page.
}
\references{

  Wikipedia contributors, \dQuote{MasterChef Australia (series 6),}
  Wikipedia, The Free Encyclopedia,
  \url{https://en.wikipedia.org/w/index.php?title=MasterChef_Australia_(series_6)&oldid=758432561}
  (accessed January 5, 2017).
  
}
\seealso{\code{\link{ggrl}}}
\examples{

a1 <- indep(equalp(masterchef[[1]]))            # equal strengths
a2 <- indep(masterchef_maxp)               # MLE
a3 <- indep(masterchef_constrained_maxp)   # constrained MLE

\dontrun{ # takes too long
like_series(a1, masterchef)
like_series(a2, masterchef)
like_series(a3, masterchef)
}
}
\keyword{datasets}
