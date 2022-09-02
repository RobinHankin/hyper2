\name{interzonal}
\alias{interzonal}
\alias{interzonal_maxp}
\alias{interzonal_table}
\alias{interzonal_collusion}
\alias{interzonal_collusion_maxp}
\alias{curacao}
\alias{curacao3}
\alias{stockholm1962}
\alias{collusion}
\alias{interzonal_collusion}
\docType{data}
\title{1963 World Chess Championships}
\description{
Likelihood functions for players' strengths in the fifth Interzonal
tournament which occurred as part of the 1963 Chess world Championships
in Stockholm, 1962.
}
\details{

  (there are three chess datasets in the package, documented at
  \code{interzonal.Rd} [the 1963 World championship], \code{kka.Rd}
  [Karpov-Kasparov-Anand dataset], and \code{chess.Rd}
  [rock-paper-scissors using Topalov-Anand-Karpov])

The 1963 World Chess Championship was notable for allegations of Soviet
collusion.  Specifically, Fischer publicly alleged that certain Soviet
players had agreed in advance to draw all their games.  The championship
included an \dQuote{interzonal} tournament in which 23 players competed
in Stockholm; and a \dQuote{Candidates} tournament in which 8 players
competed in Curacao.

Likelihood functions \code{interzonal} and \code{interzonal_collusion}
are created by files \file{inst/interzonal.Rmd}, which is heavily
documented and include some analysis.  Object \code{interzonal} includes
a term for drawing, (\dQuote{\code{draw}}), assumed to be the same for
all players; object \code{interzonal_collusion} includes in addition to
\code{draw}, a term for the drawing in Soviet-Soviet matches,
\dQuote{\code{coll}}.

Some other analysis is given in files
\code{inst/curacao11962_threeplayers.R} and
\code{inst/curacao1962_threeplayers_rest_monster.Rmd}.

}
\seealso{\code{\link{chess}},\code{\link{karpov_kasparov_anand}}}
\examples{
pie(interzonal_maxp)

# samep.test(interzonal,c("Fischer","Geller")) # takes too long
}
\keyword{datasets}
