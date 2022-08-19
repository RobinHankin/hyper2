\name{pairwise}
\alias{pairwise}
\alias{zermelo}
\alias{home_away}
\alias{home_away3}
\alias{white_draw}
\alias{white_draw3}
\title{Pairwise comparisons}
\description{\loadmathjax

  Function \code{pairwise()} takes a matrix of pairwise comparisons and
  returns a \code{hyper2} likelihood function.  Function
  \code{zermelo()} gives a standard iterative procedure for likelihood
  maximization of pairwise Bradley-Terry likelihoods (such as those
  produced by function \code{pairwise()}).

  Function \code{home_away()} takes two matrices, one for home wins and
  one for away wins.  It returns a \code{hyper2} support function that
  includes a home advantage ghost.  Function \code{home_away3()} is the
  same, but returns a \code{hyper3} object.  A complex matrix is
  interpreted as real parts being the home wins and imaginary parts away
  wins.

  Function \code{white_draw3()} returns a \code{hyper3} likelihood
  function for pairwise comparisons, one of whom has a home team-type
  advantage (white player in the case of chess).  It is designed to
  work with an array of dimensions \mjeqn{n\times n\times 3}{n*n*3},
  where \mjseqn{n} is the number of players.  It is used in
  \code{inst/kka.Rmd} to create \code{chess3} likelihood function.
      
}
\usage{
pairwise(M)
zermelo(M, maxit = 100, start, tol = 1e-10, give = FALSE)
home_away(home_games_won, away_games_won)
home_away3(home_games_won, away_games_won,lambda)
white_draw3(A,lambda,D)
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
  \item{lambda}{The home ground advantage
    (or white advantage in chess)}
  \item{D}{Weight of draw}
  \item{A}{Array of dimension \code{n*n*3}, with \code{A[,,i]}
  corresponding to white wins, white draws, and white losses for
  \code{i=1,2,3}.  The canonical example would be \code{kka_array},
  see \code{inst/kka.Rmd} for details}
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
\code{inst/zermelo.Rmd} and also \code{inst/karate.Rmd}.  Functions
\code{home_away()} and \code{home_away3()} are described and used in
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


M2 <- matrix(c(NA,19+2i,17,11+2i,16+5i,NA,12+4i,12+6i,12+2i,19+10i,
NA,12+4i,11+2i,16+2i,11+7i,NA),4,4)
teams <- LETTERS[1:4]
dimnames(M2) <- list("@home" = teams,"@away"=teams)
home_away(M2)
# home_away3(M2,lambda=1.2)  # works but takes too long (~3s)
home_away3(M2[1:3,1:3],lambda=1.2) 

M <- kka_array[,,1] + 1i*kka_array[,,3] # ignore draws
home_away(M)
# home_away3(M,lambda=1.3)  # works but takes too long (~3s)

}
