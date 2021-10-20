\name{psubs}
\alias{psubs}
\alias{psubs_single}
\alias{psubs_names}
\alias{psubs_pnames}
\title{Substitute players of a hyper2 object}
\description{Given a hyper2 object, substitute some players}
\usage{
psubs(H, from, to)
psubs_single(H, from, to)
}
\arguments{
  \item{H}{hyper2 object}
  \item{from,to}{Character vector of players to substitute and their
    substitutes}
}
\details{

Function \code{psubs()} substitutes one or more player names, replacing
player \code{from[i]} with \code{to[i]}.  If argument \code{to} is
missing, all players are substituted, the second argument taken to be
the replacement: interpret \code{psubs(H,vec)} as
\code{psubs(H,from=pnames(H),to=vec)}.

Compare \code{pnames<-()}, which can only add players, or reorder
existing players.

Function \code{psubs_single()} is a low-level helper function that takes
a single player and its substitute; it is not intended for direct use.
}

\value{Returns a hyper2 object}
\author{Robin K. S. Hankin}
\examples{

psubs(icons,c("L","NB"),c("London","Norfolk Broads"))

rhyper2() \%>\% psubs(letters,LETTERS)   # ignore i,j,k,...,z

psubs(icons,tolower(pnames(icons)))

}

