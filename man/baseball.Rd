\name{baseball}
\alias{baseball}
\alias{baseball_maxp}
\alias{baseball_table}
\docType{data}
\title{Baseball results, following Agresti}
\description{Results from repeated games among seven baseball teams,
  following Agresti}
\usage{data(baseball)}
\format{A \code{hyper2} object that gives a likelihood function}
\details{

Agresti discusses results from seven baseball teams in the 1987 season
of the Eastern Division of the American League.  

A results table and likelihood function is given in the package as
\code{baseball_table} and \code{baseball} respectively.  The maximum
likelihood estimate is given as \code{baseball_maxp}, but can be
reproduced by \code{maxp(baseball)}.
  
\generatedby{inst/home_advantage.Rmd}{baseball.rda}{https://robinhankin.github.io/hyper2/home_advantage.html}

}
\references{
  A. Agresti 2002. \dQuote{Categorical data analysis}.  John Wiley and
  Sons;   p437
}
\seealso{
  \code{\link{hyper3}}

  \seegithub
}
\examples{
baseball_table
baseball_table[1:3,1:3] 
home_away3(baseball_table[1:3,1:3], 1.3)

}

