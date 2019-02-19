\name{karpov_kasparov_anand}
\alias{karpov_kasparov_anand}
\alias{kka}
\docType{data}
\title{Karpov, Kasparov, Anand}
\description{
A likelihood function for the strengths of three chess players: Karpov,
Kasparov, and Anand}
\usage{data(karpov_kasparov_anand)}
\details{
  The strengths of chess players may be assessed using the generalized
  Bradley-Terry model.  The \code{karpov_kasparov_anand} likelihood
  function allows one to estimate the players' strengths, propensity to
  draw, and also the additional strength conferred by playing white.

  The likelihood function is created by file
  \code{inst/karpov_kasparov_anand.R}, which is heavily documented.

  The reason that the players are different from those in the
  \code{chess} dataset is that the original data does not seem to be
  available any more.

  Dataset \code{kka} refers to matches between three chess players
 (Kasparov, Karpov, Anand).  Data drawn from chessgames.com,
 specifically

 \url{http://www.chessgames.com/perl/ezsearch.pl?search=karpov+vs+kasparov}

 Note that the database allows one to sort by white wins or black wins
 (there is a \Quote{refine search} tab at the bottom).  Some searches
 have more than one page of results.
 
 Numbers here downloaded 17 February 2019.  Note that only
 \sQuote{classical games} are considered here (rapid and exhibition
 games being ignored).





}
\seealso{\code{\link{chess}}}
\examples{
data(karpov_kasparov_anand)
maxp(karpov_kasparov_anand)
}
\keyword{datasets}
