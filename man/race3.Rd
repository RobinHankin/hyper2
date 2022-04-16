\name{race3}
\alias{race3}
\alias{rracehyper3}
\alias{num3}
\alias{den3}
\alias{rwinner3}
\alias{race_to_hyper3}
\title{Various functionality for races and hyper3 likelihood functions}
\description{
  Various functions for calculating the likelihood function for order
statistics in the context of hyper3 likelihood functions.  Compare
\code{ggol()} for \code{hyper2} objects.    Used in the
\code{constructor()} suite of analysis.
}
\usage{
num3(v)
den3(v)
race_to_hyper3(v,nonfinishers=NULL)
rwinner3(pn,ps)
rrace3(pn,ps)
rracehyper3(n=4,size=9,ps=NULL,races=3)
}

\arguments{
  \item{v}{Character vector showing finishing order.}
  \item{nonfinishers}
  \item{pn}{object}
  \item{pn}{object}
  \item{n,size,races}
}
\details{

  These functions are intended to work with \code{hyper3} objects, as
  indicated by the \code{3} in their names.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{rrank}},\code{\link{ordertable2supp}},\code{\link{ordertrans}}}
\examples{


## player numbers (two "a"s, four "b"s, two "c"s and a "d"):
pn <- c(a=2,b=4,c=2,d=1) 

ps <- c(a=0.3, b=0.1,c=0.2,d=0.4)  # player strengths    

rwinner3(pn,ps)              # winner, randomly chosen
rrace3(pn,ps)                 # A random race
race_to_hyper3(rrace3(pn,ps))  # a hyper3 object
rrace3()                        # a hyper3 object for multiple races

}


