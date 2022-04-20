\name{pairwise}
\alias{pairwise}
\alias{zermelo}
\alias{home_away}
\title{Pairwise comparisons}
\description{

  Function \code{pairwise()} takes a matrix of pairwise comparisons and
  returns a \code{hyper2} likelihood function.  Function
  \code{zermelo()} gives a standard iterative procedure for likelihood
  maximization of pairwise Bradley-Terry likelihoods (such as those
  produced by function \code{pairwise()}).

  Function \code{home_away()} takes two matrices, one for home wins and
  one for away wins.  It returns a \code{hyper2} support function that
  includes a home advantage ghost.
  
}
\usage{
pairwise(M)
zermelo(M, maxit = 100, start, tol = 1e-10, give = FALSE)
home_away(home_games_won, away_games_won)
}
\arguments{
  \item{M}{Matrix of pairwise comparison results}
  \item{maxit}{Maximum number of iterations}
  \item{start}{Starting value for iteration; if missing, use
    \code{equalp()}}
  \item{tol}{Numerical tolerance for stopping criterion}
  \item{give}{Boolean with default \code{FALSE} meaning to return the
    evaluate and \code{TRUE} meaning to return all iterations}
\item{home_games_won, away_games_won}{Matrices showing home games won
  and away games won}
}
\details{
  In function \code{zermelo()}, the diagonal is disregarded.

  If \code{home_games_won} is complex, then the real parts of the
  entries are interpreted as home games won, and the imaginary parts as
  away games won.
  
}
\references{
  \itemize{
    \item D. R. Hunter 2004. \dQuote{MM algorithms for generalized
  Bradley-Terry models}. \emph{The Annals of Statistics}, volume 32,
number 1, pages 384--406
\item S. Borozki and others 2016.  \dQuote{An application of incomplete pairwise
  comparison matrices for ranking top tennis
  players}. \code{arXiv:1611.00538v1} \code{10.1016/j.ejor.2015.06.069}
}
}
\author{Robin K. S. Hankin}
\note{

An extended discussion of \code{pairwise()} is given in
\code{inst/zermelo.Rmd} and also \code{inst/karate.Rmd}.  Function
\code{home_away()} is described and used in
\code{inst/home_advantage.Rmd}.

}
\seealso{\code{\link{maxp}}}
\examples{ #Data is the top 5 players from Borozki's table 1

M <- matrix(c(
0,10,0, 2,5,
4, 0,0, 6,6,
0, 0,0,15,0,
0, 8,0, 0,7,
1 ,0,3, 0,0
),5,5,byrow=TRUE) 
players <-  c("Agassi","Becker","Borg","Connors","Courier")
dimnames(M) <- list(winner=players,loser=players)
M
# e.g. Agassi beats Becker 10 times and loses 4 times
pairwise(M)
zermelo(M)
# maxp(pairwise(M))  # should be identical (takes ~10s to run)

}
