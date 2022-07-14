\name{ordervec2supp3}
\alias{race3}
\alias{num3}
\alias{den3}
\alias{char2nv}
\alias{ordervec2supp3}
\alias{ordertable2supp3}
\alias{args2ordervec}
\alias{race_to_hyper3}
\alias{cheering}
\alias{cheering3}
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
char2nv(x)
ordervec2supp3(v,nonfinishers=NULL)
ordertable2supp3(a)
cheering3(v,e,help)
args2ordervec(...)
}
\arguments{
  \item{v}{Ranks in the form of a character vector.  Element \code{v[1]}
    is the first-placed competitor, element \code{v[2]} the second, and
    so on}
  \item{nonfinishers}{Character vector (a set) showing players that did
    not finish}
  \item{a}{An ordertable}

  \item{e,help}{Parameters controlling non-independence in
    \code{cheering3()} with \code{e} a named integer vector specifying
    equivalence classes of the competitors: names correspond to the
    competitors, values to their equivalence class, and \code{help} a
    numeric vector with entries corresponding to the equivalence classes
    of \code{e} and values the strength of the support}
  \item{x}{A character vector of competitors}
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

  Function \code{char2nv()} takes a character vector and returns a named
  vector with entries corresponding to their names' counts.  It is used
  in the extraction and replacement methods for \code{hyper3} objects.

  Function \code{cheering3()} is a generalization of
  \code{ordervec2supp3()}.  Competitors who are not mentioned in
  argument \code{e} are assumed to be in an equivalence class of size 1,
  that is, they are not supported (or indeed suppressed) by anyone else:
  they are singletons in the terminology of Hankin (2006).  Extensive
  discussions are presented at \code{inst/plackett_luce_monster.Rmd} and
  \code{inst/eurovision.Rmd}.
  
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


## Race: six competitors a-f finishing in alphabetical order.  Mutually
## supporting groups: (acd), (bf), (e).  Competitor "e" is not
## suppported by anyone else (he is a singleton) so does not need to be
## mentioned in argument 'e' and there are only two helpfulnesses to be
## considered: that of (acd) and that of (bf), which we will take to be
## 1.88 and 1.1111 respectively:


cheering3(v=letters[1:6],e=c(a=1,c=1,b=2,d=1,e=2),help=c(1.88,1.1111))

}


