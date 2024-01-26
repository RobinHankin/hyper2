\name{keep}
\alias{keep}
\alias{retain}
\alias{discard}
\alias{drop}
\alias{keep_flawed}
\alias{retain_flawed}
\alias{discard_flawed}
\alias{drop_flawed}
\alias{keep_flawed2}
\alias{discard_flawed2}
\title{Keep or discard players}
\description{Flawed functionality to keep or discard subsets of the players in
  a \code{hyper2} object or order table.}
\usage{
discard_flawed2(x, unwanted,...)
keep_flawed(H, wanted)
discard_flawed(H, unwanted)
}
\arguments{
  \item{H}{A \code{hyper2} object}
  \item{x}{An order table}
  \item{wanted,unwanted}{Players to keep or discard.  May be character
    or integer or logical}
  \item{...}{Further arguments passed to
    \code{wikitable_to_ranktable()}, notably \code{points}}
}
\details{

  \strong{Do not use these functions.  They are here as object lessons
  in poor thinking.  To work with a subset of competitors, see the
  example at \link{as.ordertable}.}

  Functions \code{keep_flawed2()} and \code{discard_flawed2()} take an
  order table and keep or discard specified rows, returning a reduced
  order table.  This is not a trivial operation.
  
  Functions \code{keep_flawed()} and \code{discard_flawed()} will either
  keep or discard players specified in the second argument.  It is not
  clear to me that these functions have any reasonable probabilistic
  interpretation and file \code{inst/retain.Rmd} gives a discussion.

  Given a wikitable or ordertable, it is possible to create a likelihood
  function based on a subset of rows using the \code{incomplete=TRUE}
  argument; see the example at \code{?ordertable2supp}.  But this method
  is flawed too because it treats non-finishers as if they finished in
  the order of their rows.

  Function \code{as.ordertable()} is the correct way to consider a
  subset of players in a wikitable.
  
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{ordertable2supp}},\code{\link{tidy}}}
\examples{

maxp(icons)
discard_flawed(icons,c("OA","WAIS"))

\dontrun{# (takes too long)
data("skating")
maxp(skating)[1:4]      # numbers work, keep the first four skaters
maxp(keep_flawed(skating,pnames(skating)[1:4])) # differs!
}


}
