\name{table_tennis}
\alias{table_tennis}
\alias{table_tennis_serve}
\alias{table_tennis_maxp}
\alias{ping_pong}
\docType{data}
\title{Match outcomes from repeated table tennis matches}
\description{Match outcomes from repeated singles table tennis matches}
\usage{data(table_tennis)}
\format{
A likelihood function corresponding to the match outcomes listed below.
}
\details{

There are four players, A, B, and C, who play singles table tennis
matches with the following results:

\itemize{
  \item A vs B, A serves, 5-1
  \item A vs B, B serves, 1-3
  \item A vs C, A serves, 4-1
  \item A vs C, C serves, 1-2
}

Object \code{table_tennis} is a \code{hyper2} log-likelihood function
for these observations; \code{S} is the strength of the serving ghost.
Probability vector \code{table_tennis_maxp} is the maximum likelihood
estimate.

\generatedbynolink{inst/table_tennis_serve.Rmd}{table_tennnis.rda}
  
}

\examples{
data(table_tennis)
dotchart(table_tennis_maxp)

specificp.gt.test(table_tennis, "S", 0)

}
\seealso{\seegithub}
\keyword{datasets}
