\name{retain}
\alias{retain}
\title{Given a wikitable or order table, retain selected competitors}
\description{
Given a wikitable or order table, sometimes we wish to retain only
certain competitors (typically the strongest).  This is not a trivial
task.
}
\usage{
retain(x, wanted)
}
\arguments{
  \item{x}{order table or wikitable}
  \item{wanted}{competitors to keep or discard}
}

\details{
Retains the specified competitors and discards the rest via a modified
order table; see \code{inst/retain.Rmd} for details and a discussion.
}
\value{Returns an order table}
\author{Robin K. S. Hankin}
\seealso{\code{\link{keep}}}
\examples{
ordertable2supp(retain(skating_table,1:5))
keep(ordertable2supp(skating_table),1:5)
}
