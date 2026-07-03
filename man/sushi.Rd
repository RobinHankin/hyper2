\name{sushi}
\alias{sushi}
\alias{sushi_table}
\alias{sushi_maxp}
\alias{sushi_eq_classes}
\docType{data}
\title{Sushi dataset}
\description{
  Dataset from the \emph{preflib} website, discussing sushi preferences.
}
\usage{data(sushi)}

\details{

Sushi preference dataset.

\itemize{
  \item Object \code{sushi_table} is a database of sushi preferences;
  each row is a judge and the rows are rank vectors of the different
  sushi types.
  \item Object \code{sushi} is a \code{hyper3} likelihood function at the estimated
  value of lambda
  \item Object \code{sushi_maxp} is the maximum likelihood estimate for
  the Bradley-Terry strengths of the sushis, at the estimate for lambda.
  \item \code{sushi_eq_classes} is a named vector giving the
  equivalences classes of sushi.  Class 1 is tuna, class 2 is everything
  else.
  }

\generatedbynolink{inst/sushi.Rmd}{sushi.rda}

}

\examples{
sushi
sushi_maxp
}
