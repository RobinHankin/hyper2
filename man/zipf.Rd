\name{zipf}
\alias{zipf}
\title{Zipf's law}
\description{\loadmathjax

A very short function that reproduces Zipf's law: a harmonic rank-probability
distribution, formally

\mjdeqn{p(i)=\frac{i^{-1}}{\sum_{i=1}^{N} i^{-1}},\qquad i=1,\ldots,N}{omitted}

}
\usage{zipf(n)}
\arguments{\item{n}{Integer; if a hyper2 object is supplied this is
    interpreted as \code{size(n)}}}
\value{Returns a numeric vector summing to one}
\author{Robin K. S. Hankin}
\seealso{\code{\link{knownp.test}}}
\examples{

zipf(icons)
}