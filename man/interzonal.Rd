\name{interzonal}
\alias{interzonal}
\alias{curacao}
\alias{curacao3}
\alias{stockholm1962}
\alias{collusion}
\alias{interzonal_collusion}
\docType{data}
\title{1963 World Chess Championships}
\description{
Likelihood functions for players' strengths in the fith Interzonal
tournament which occurred as part of the 1963 Chess world Championships
in Stockholm, 1962.
}
\details{

  The 1963 World Chess Championship was notable for allegations of
  Soviet collusion.  Specifically, Fischer publicly alleged that
  certain Soviet players had agreed in advance to draw all their games.
  The championship included an \dQuote{interzonal} tournament in which
  23 players competed in Stockholm; and a \dQuote{Candidates} tournament
  in which 8 players competed in Curacao.

  Likelihood functions \code{interzonal} and \code{interzonal_collusion}
  are created by files \file{inst/collusion_stockholm1962.R}, which are
  heavily documented and include some analysis.  Object
  \code{interzonal} includes a term for drawing, (\dQuote{\code{draw}}),
  assumed to be the same for all players; object
  \code{interzonal_collusion} includes in addition to \code{draw}, a
  term for the drawing in Soviet-Soviet matches, \dQuote{\code{coll}}.

  Likelihood function \code{curacao} is created in heavily documented
  file \file{inst/curacao11962_threeplayers.R}.  The function includes a
  term for drawing, and a term for collusive drawing, \code{colldraw}.

}
\seealso{\code{\link{chess}},\code{\link{karpov_kasparov_anand}}}
\examples{
data(interzonal)
dotchart(maxp(interzonal,n=2)) # n=2 for speed

maxp(interzonal_collusion,n=2)

curacao
}
\keyword{datasets}
