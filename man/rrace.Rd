\name{rrace}
\alias{rrace}
\title{A random race with given BT strengths}
\description{
Returns a rank vector suitable for interpretation with \code{race()}.
}
\usage{rrace(strengths)}
\arguments{
  \item{strengths}{
    Named vector with names being players and values being their
    Bradley-Terry strengths}
}
\details{
  Uses a simple recursive system to generate the ranks.
}
\value{
Returns a character vector with entries corresponding to the competitor.
The first element is the winner, the second the runner-up, and so on,
until the final element is the last to cross the finishing line.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{rrace3}},\code{\link{hyper2}}}
\examples{

o <- c(a=0.4, b=0.3, c=0.2, d=0.1)
rrace(o)

rankvec_likelihood(rrace(o))

D <- t(replicate(10,rrace(o))) # 10 races
H <- hyper2()
for(i in seq_len(nrow(D))){H <- H+rankvec_likelihood(D[i,])}
maxp(H)  # should be close to o
}
