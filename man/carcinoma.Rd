\name{carcinoma}
\alias{carcinoma}
\alias{carcinoma_table}
\alias{carcinoma_maxp}
\alias{carcinoma_count}
\docType{data}
\title{Carcinoma dataset discussed by Agresti}
\description{A dataset considered by Agresti.  Seven clinicians are
  asked whether they see evidence for carcinoma on different patients.}
\usage{data(carcinoma)}
\format{A \code{hyper2} object that gives a likelihood function}
\details{

Object \code{carcinoma_table} is drawn from Agresti.  The first seven
columns correspond to the seven clinicians A-G, the next is the count
of observations, and the remaining columns are fitted values according
to different models discussed by Agresti.

Object \code{carcinoma} is a likelihood function (of class \code{lsl})
on the Bradley-Terry strengths of the seven clinicians.  The
clinicians diagnosed the presence or absence of carcinoma on a total
of 118 patients in a blind rating scheme.  The maximum likelihood
estimator for the clinicians' Bradley-Terry strengths is given as
\code{carcinoma_maxp}, which is computationally expensive to find.
The package also includes \code{carcinoma_count}, which is a different
estimator for the Clinicians' \acronym{BT} strengths.

\generatedby{inst/carcinoma.Rmd}{carcinoma.rda}{https://robinhankin.github.io/hyper2/carcinoma.html}

}
\references{

A. Agresti, 2002.  "Categorical data analysis".  John Wiley and
Sons. Table 13.1, p542.
 }
 \seealso{
   \code{\link{race3}},
   \code{\link{hepatitis}}

   \seegithub
 }
\examples{
pie(carcinoma_maxp)
}

