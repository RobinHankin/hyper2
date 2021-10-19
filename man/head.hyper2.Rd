\name{head.hyper2}
\alias{head.hyper2}
\title{First few terms of a  distribution}
\description{First few terms in a hyperdirichlet distribution}
\usage{\method{head}{hyper2}(x, ...)}
\arguments{
  \item{x}{Object of class \code{hyper2}}
  \item{\dots}{Further arguments, passed to \code{head()}}
}
\details{Function is  \code{x[head(brackets(x), ...)]}}
\value{Returns a \code{hyper2} object}
\author{Robin K. S. Hankin}
\examples{

p <- zipf(5)
names(p) <- letters[1:5]
H <- rank_likelihood(rrank(20,p))
head(H)


}
