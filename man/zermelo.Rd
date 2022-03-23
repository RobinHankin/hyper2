\name{zermelo}
\alias{zermelo}
\alias{pairwise}
\title{Pairwise comparisons}
\description{

  Function \code{pairwise()} takes a matrix of pairwise comparisons and
  returns a \code{hyper2} likelihood function.  Function
  \code{zermelo()} gives a standard iterative procedure for likelihood
  maximization of pairwise Bradley-Terry likelihoods (such as those
  produced by function \code{pairwise()}).

}
\usage{
zermelo(M, maxit = 100, start, tol = 1e-10, give = FALSE)
}
\arguments{
  \item{M}{Matrix of pairwise comparison results}
  \item{maxit}{Maximum number of iterations}
  \item{start}{Starting value for iteration; if missing, use
    \code{equalp()}}
  \item{tol}{Numerical tolerance for stopping criterion}
  \item{give}{Boolean with default \code{FALSE} meaning to return the
    evaluate and \code{TRUE} meaning to return all iterations}
}
\details{
  In function \code{zermelo()}, the diagonal is disregarded.

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
An extended discussion is given in \code{inst/zermelo.Rmd} and also
\code{inst/karate.Rmd}. 
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
