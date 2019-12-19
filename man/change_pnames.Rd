\name{change_pnames}
\alias{change_pnames}
\alias{all_pnames}
\title{Change pnames of a hyper2 object}
\description{Change the pnames attribute of a hyper2 object}
\usage{
change_pnames(H, new_pnames)
all_pnames(L)
}
\arguments{
  \item{H}{Object of class hyper2}
  \item{L}{List of hyper2 objects}
  \item{new_pnames}{Character vector of the new pnames}
}
\author{Robin K. S. Hankin}
\note{

  Function \code{change_pnames()} is not straightforward.  The intent is
  to enable one to \dQuote{add} [that is, combine two log-likelihood
  functions corresponding to two sets of independent observations into a
  single log-likelihood function] \code{hyper2} objects with
  non-identical pnames attributes in a sensible way.

  The standard use for \code{change_names()} is to add new players to a
  hyper2 object: so the new names must be a superset of the old,
  although the order may differ.

  Function \code{all_pnames()} returns the union of the pnames of the
  hyper2 objects sent to it
  
  A non-trivial use-case is given in \code{inst/formula1.Rmd}.

}
\examples{

##   H1 is binomial, a vs b, a wins 3, b wins 6.
##   H2 is binomial, b vs c, b wins 2, c wins 4.

H1 <- hyper2(list(1,2,1:2),c(3,6,-9),pnames=letters[1:3])
H2 <- hyper2(list(1,2,1:2),c(2,4,-6),pnames=letters[2:3])

\dontrun{
H1+H2  # fails: pnames differ
}

# Combine H1 and H2 assuming independence:

H <- change_pnames(H1,letters[1:3]) + change_pnames(H2,letters[1:3])

## sometimes it is better to change in place, as in

H1 \%<>\% change_pnames(letters[1:3])


## changing pnames can have unexpected behaviour:

H3 <- H1
H3 \%<>\% change_pnames(letters[3:1])

# H1 and H3  are algebraically identical; but:

H1 == H3 # false

## Variables H1 and H3 are different!  This is by design. The pnames
## attribute is just syntatic sugar and mathematically meaningless.

}