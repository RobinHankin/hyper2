\name{matrix2supp}
\alias{matrix2supp}
\alias{saffy}
\alias{volley}
\title{Convert a matrix to a likelihood function}
\description{

  Functions to convert matrix observations to likelihood functions.
  Each row is an observation of some kind, and each column a player.

  Function \code{ordertable2supp()} is documented separately at
  \code{ordertable2supp}.
}
\usage{
saffy(M)
volley(M)
}

\arguments{\item{M}{A matrix of observations}}

\details{

Two functions are documented here:

\itemize{

  \item \code{saffy()}, which converts a matrix of restricted choices
   into a likelihood function; it is named for Saffron O'Neill.  The
   canonical example would be Saffron's climate change dataset,
   documented at \code{icons}.  Function \code{saffy()} returns the
   appropriate likelihood function for the dataset.


  \item \code{volley()}, which converts a matrix of winning and losing
  team members to a likelihood function.  The canonical example is the
  volleyball dataset.  Each row is a volleyball game; each column is a
  player.  An entry of 0 means \dQuote{on the losing side}, an entry of
  1 means \dQuote{on the winning side}, and an entry of \code{NA} means
  \dQuote{did not play}.
  }

}

\author{Robin K. S. Hankin}
\seealso{\code{\link{icons}},\code{\link{volleyball}}}
\examples{

icons == saffy(icons_table)  # should be TRUE

volley(volleyball_table) == volleyball # also should be TRUE

}