\name{maxp}
\alias{maxp}
\title{Maximum likelihood estimation}
\description{Find the maximum likelihood estimate for p}
\usage{
maxp(H, startp=NULL, give=FALSE, ...)
}
\arguments{
  \item{H}{A \code{hyper2} object}
  \item{startp}{A vector of probabilities}
  \item{give}{Boolean, with default \code{FALSE} meaning to return just
    the evaluate (including fillup), and \code{TRUE} meaning to return
    the entire formal output of the optimization routine}
  \item{...}{Further arguments which \code{maxp()} passes to
    \code{constrOptim()}}
}
\details{
  Function \code{maxp()} returns the maximum likelihood estimate for
  \code{p}, which has the unit sum constraint implemented.

  The function does not work for the output of \code{ggol()} nor the
  \code{masterchef_series6} dataset.  These require a bespoke
  optimization as shown in the vigentte.
}
\author{
Robin K. S. Hankin
}
\note{
This functionality is peculiarly susceptible to off-by-one errors.
}
\seealso{\code{\link{gradient}}}
\examples{

data(chess)
maxp(chess)

}
