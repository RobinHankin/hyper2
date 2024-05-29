\name{ordervec2supp3}
\alias{race3}
\alias{num3}
\alias{den3}
\alias{char2nv}
\alias{ordervec2supp3}
\alias{ordervec2supp3a}
\alias{ordertable2supp3}
\alias{args2ordervec}
\alias{race_to_hyper3}
\alias{cheering}
\alias{cheering3}
\alias{rankvec_likelihood3}
\title{Various functionality for races and hyper3 likelihood functions}
\description{
Various functions for calculating the likelihood function for order
statistics in the context of hyper3 likelihood functions.  Compare
\code{ggol()} for \code{hyper2} objects.  Used in the
\code{constructor()} suite of analysis.
}
\usage{
num3(v,helped=NULL,lambda=1)
den3(v,helped=NULL,lambda=1)
char2nv(x)
ordervec2supp3(v,nonfinishers=NULL)
ordervec2supp3a(v,nonfinishers=NULL,helped=NULL,lambda=1)
rankvec_likelihood3(v,nonfinishers=NULL)
ordertable2supp3(a)
cheering3(v,e,help,nonfinishers=NULL)
args2ordervec(...)
}
\arguments{
  \item{v}{Ranks in the form of a character vector.  Element \code{v[1]}
    is the first-placed competitor, element \code{v[2]} the second, and
    so on.  For example, \code{ordervec2supp3(c('b','b','a','c','a'))}}
  \item{nonfinishers}{Character vector (a set) showing players that did
    not finish.  See details section and examples}
  \item{a}{An ordertable}
  \item{helped}{vector of entities being helped}
  \item{e,help,lambda}{Parameters controlling non-independence with
    \code{e} a named integer vector specifying equivalence classes of
    the competitors: names correspond to the competitors, values to
    their equivalence class, and \code{help} a numeric vector with
    entries corresponding to the equivalence classes of \code{e} and
    values the strength of the support}
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
  order of finishing [i.e. a rank statistic], and returns a generalized
  Plackett-Luce support function in the form of a \code{hyper3} object.
  It can take the output of \code{args2ordervec()} or \code{rrace3()}.
  For example:

  \code{ordervec2supp3(c("a","b"),nonfinishers=c("a","b"))}

  corresponds to a race between two twins of strength \code{a} and two
  twins of strength \code{b}, with only one of each pair finishing;
  \code{a} comes first and \code{b} comes second; symbolically

  \deqn{a\succ b\succ\left\lbrace a,b\right\rbrace\longrightarrow
    \mathcal{L}(a,b\left|a+b=1\right.)=\frac{a}{2a+2b}\cdot\frac{b}{a+2b}}{omitted}

  Further,

  \code{ordervec2supp3(c("a","b"),c("a","b","c"))}

  corresponds to adding a singleton competitor of strength \code{c} who
  did not finish:

  \deqn{a\succ b\succ\left\lbrace a,b,c\right\rbrace\longrightarrow
    \mathcal{L}(a,b,c\left|a+b+c=1\right.)=\frac{a}{2a+2b+c}\cdot\frac{b}{a+2b+c}}{omitted}

  (observe that this likelihood function is informative about
  \eqn{c}).  See the examples section below.  Experimental function
  \code{ordervec2supp3a()} is a generalized version of
  \code{ordervec2supp3()} that allows for cheering effects.

  Functions \code{num3()} and \code{den3()} are low-level helper
  functions that calculate the numerator and denominator for
  Plackett-Luce likelihood functions with clones; used in
  \code{ordervec2supp3()} and \code{ordervec2supp3a()}.

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
\note{
  Function \code{ordervec2supp3()} is mis-named [it takes a \emph{rank}
  vector, not an \emph{order} vector]; it will be renamed
  \code{rankvec_likelihood3()}, eventually.
}  
\seealso{\code{\link{ordertable2supp}},\code{\link{ordertrans}}}
\examples{


ordervec2supp3(c("a","a","b","c","a","b","c"))
ordervec2supp3(rrace3())
ordervec2supp3(c("a","b"),nonfinishers=c("a","b"))  # a > b >> {a,b}



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




## Another race: four competitors, including two clones of "a", and two
## singletons "b" and "c".  Here "a" helps his clone at 1.88; and "b"
## and "c" help one another at 1.111:

cheering3(v=c("a","b","a","c"),e=c(a=1,b=2,c=2),help=c(1.8,1.111))


## Same race as above but this time there are two clones of "b", one of
## whom did not finish:

cheering3(v=c("a","b","a","c"),e=c(a=1,b=2,c=2),help=c(1.8,1.111),"b")


## Most common case would be that the clones help each other but noone
## else:

cheering3(v=c("a","b","a","c"),e=c(a=1,b=2,c=3),help=c(1.8,1.111,1),"b")



}


