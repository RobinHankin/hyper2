\name{tidy}
\alias{tidy}
\title{Tidy up a hyper2 object}
\description{Tidy up a hyper2 object by removing players about which we
  have no information}
\usage{tidy(H)}
\arguments{\item{H}{A \code{hyper2} object}}
\details{

  Function \code{tidy(H)} returns a hyper2 object mathematically
  identical to \code{H} but with unused players (that is, players that
  do not appear in any bracket) removed.  Players about which \code{H}
  is uninformative are removed from the \code{pnames} attribute.

  Note that idiom \code{pnames(H) <- foo} can also be used to manipulate
  the \code{pnames} attribute.


}
\author{Robin K. S. Hankin}
\examples{


H <- hyper2(pnames=letters)
H["a"] <- 1
H["b"] <- 2
H[c("a","b")] <- -3

pnames(H)
pnames(tidy(H))

H == tidy(H)  # should be TRUE



}
