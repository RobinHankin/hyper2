\name{combine}
\alias{combine}
\title{
Function to combine two hyper2 objects
}
\description{
Function to combine two hyper2 objects, possibly with different pnames.
}
\usage{combine(H1, H2)}
\arguments{\item{H1,H2}{Objects of class hyper2}}
\value{Returns an object of class hyper2}
\author{Robin K. S. Hankin}
\note{Possibly idiom \code{H1 + H2} should return combine(H1,H2)}
\seealso{\code{\link{Ops.hyper2}}}
\examples{

y1 <- hyper2(list(1,2,3,2:3),1:4,pnames=letters[1:3])
y2 <- hyper2(list(1,2,3,1:2),1:4,pnames=letters[2:4])
\dontrun{y1+y2}

combine(y1,y2)


}