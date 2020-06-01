\name{keep}
\alias{keep}
\alias{discard}
\alias{keep_flawed}
\alias{discard_flawed}
\title{Keep or discard players}
\description{Functionality to keep or discard subsets of the players in
  a \code{hyper2} object or order table.}
\usage{
keep(x,wanted,...)
discard(x,unwanted,...)
keep_flawed(H, wanted, tidy=TRUE)
discard_flawed(H, unwanted, tidy=TRUE)
}
\arguments{
  \item{H}{A \code{hyper2} object}
  \item{x}{An order table}
  \item{tidy}{Boolean, with default \code{TRUE} meaning to return a
    mathematically identical, but tidied, likelihood function}
  \item{wanted,unwanted}{Players to keep or discard.  May be character
    or integer or logical}
  \item{...}{Further arguments passed to
    \code{wikitable_to_ranktable()}, notably \code{points}}
}
\details{

  Functions \code{keep()} and \code{discard()} take an order table and
  keep or discard specified rows, returning a reduced order table.  This
  is not a trivial operation.
  
  Functions \code{keep_flawed()} and \code{discard_flawed()} will either
  keep or discard players specified in the second argument.  It is not
  clear to me that these functions have any reasonable probabilistic
  interpretation and file \code{inst/retain.Rmd} gives a discussion.

  Given a wikitable or ordertable, it is possible to create a likelihood
  function based on a subset of rows using the \code{incomplete=TRUE}
  argument; see the example at \code{?ordertable2supp}.
  
}
\author{Robin K. S. Hankin}
\note{Function \code{tidy()} is very, very, inelegant}
\seealso{\code{\link{ordertable2supp}}}
\examples{

data("oneill")
maxp(icons)
discard_flawed(icons,c("OA","WAIS"))

data("skating")
maxp(skating)[1:4]      # numbers work, keep the first four skaters
maxp(keep_flawed(skating,1:4))

pie(maxp(ordertable2supp(keep(F1_table_2017,1:9))))

}
