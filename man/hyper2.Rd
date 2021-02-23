\name{hyper2}
\alias{hyper2}
\alias{brackets}
\alias{brackets.hyper2}
\alias{powers}
\alias{powers.hyper2}
\alias{pnames}
\alias{pnames.hyper2}
\alias{pnames.suplist}
\alias{pnames<-}
\alias{pnames<-.hyper2}
\alias{size}
\alias{size.hyper2}
\alias{is.hyper2}
\alias{as.hyper2}
\alias{is_valid_hyper2}
\alias{is_constant}
\title{
Basic functions in the hyper2 package
}
\description{
Basic functions in the hyper2 package
}
\usage{
hyper2(L=list(), d=0, pnames)
\method{brackets}{hyper2}(H)
\method{powers}{hyper2}(H)
\method{pnames}{hyper2}(H)
\method{pnames}{suplist}(H)
size(H)
as.hyper2(L,d,pnames)
is.hyper2(H)
is_valid_hyper2(L,d,pnames)
is_constant(H)
}
\arguments{
  \item{H}{A hyper2 object}
  \item{L}{A list of character vectors whose elements specify
    the brackets of a hyper2 object}
  \item{d}{A vector of powers; \code{hyper2()} recycles \emph{only if}
    \code{d} is of length 1}
  \item{pnames}{
    A character vector specifying the names of \eqn{p_1} through \eqn{p_n}.}
}
\details{
These are the basic functions of the hyper2 package.  Function
\code{hyper()} is the low-level creator function; \code{as.hyper2()} is
a bit more user-friendly and attempts to coerce its arguments into a
suitable form; for example, a matrix is interpreted as rows of brackets.

Functions \code{pnames()} and \code{pnames<-()} are the accessor and
setter methods for the player names.  Length-zero character strings are
acceptable.

Function \code{is_valid_hyper2()} tests for valid input, returning a
Boolean.  Note that it is perfectly acceptable to have an element of
\code{pnames} that is not present in the likelihood function (this would
correspond to having no information about that particular player).

Function \code{size()} returns the (nominal) length \eqn{n} of
nonnegative vector \eqn{p=\left(p_1,\ldots,p_n\right)}{p=(p_1,...,p_n)}
where \eqn{p_1+\cdots+p_n=1}{p_1+...+p_n=1}.  Note that function
\code{lhyper2()} takes the vector
\eqn{p=\left(p_1,\ldots,p_{n-1}\right)}{p=(p_1,...,p_{n-1})}.
}
\note{The empty string is a perfectly valid name}
\author{Robin K. S. Hankin}
\seealso{
  \code{\link{Ops.hyper2}},
  \code{\link{Extract.hyper2}},
  \code{\link{loglik}},
  \code{\link{hyper2-package}}
}
\examples{
o <- hyper2(list("a","b","c",c("a","b"),letters[1:3]),1:5) 

# Verify that the MLE is invariant under reordering
pnames(icons) <- rev(pnames(icons))
maxp(icons) - icons_maxp # should be small

}
