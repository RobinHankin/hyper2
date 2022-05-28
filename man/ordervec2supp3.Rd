\name{ordervec2supp3}
\alias{race3}
\alias{num3}
\alias{den3}
\alias{ordervec2supp3}
\alias{ordertable2supp3}
\alias{args2ordervec}
\alias{race_to_hyper3}
\title{Various functionality for races and hyper3 likelihood functions}
\description{
Various functions for calculating the likelihood function for order
statistics in the context of hyper3 likelihood functions.  Compare
\code{ggol()} for \code{hyper2} objects.  Used in the
\code{constructor()} suite of analysis.
}
\usage{
num3(v)
den3(v)
ordervec2supp3(v,nonfinishers=NULL)
ordertable2supp3(a)
args2ordervec(...)
}
\arguments{
  \item{v}{Ranks in the form of a character vector}
  \item{nonfinishers}{Character vector (a set) showing players that did
    not finish}
  \item{a}{An ordertable}
  \item{...}{Arguments passed to \code{args2ordervec()}}
}
\details{

  Function \code{args2ordervec()} takes arguments with names
  corresponding to players, and entries corresponding to performances
  (e.g. distances thrown by a javelin, or times for completing a
  race).  It returns a character vector indicating the rank statistic.
  See examples, and also the javelin vignette.

  Function \code{ordervec2supp3()} takes character vector showing the
  order of finishing [i.e. a rank statistic], and returns a
  generalized Plackett-Luce support function in the form of a
  \code{hyper3} object.  It can take the output of
  \code{args2ordervec()} or \code{rrace3()}.  See the examples section
  below.

  Function \code{ordertable2supp3()} takes an order table (the canonical
  example is the constructors' formula 1 grand prix results, see
  \code{constructor.Rd} and returns a generalized Plackett-Luce support
  function in the form of a \code{hyper3} object.

  File \code{inst/javelin.Rmd} and \code{inst/race3.Rmd} show some
  use-cases for these functions.

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{ordertable2supp}},\code{\link{ordertrans}}}
\examples{


ordervec2supp3(c("a","a","b","c","a","b","c"))

ordervec2supp3(rrace3())

(o <- args2ordervec(a=c(1,6,9), b=c(2,3,4), c=c(1.1,11.1)))
H <- ordervec2supp3(o)
H
# equalp.test(H)   # takes too long for here


}


