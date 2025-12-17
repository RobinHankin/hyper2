\name{attemptstable2supp3}
\alias{attemptstable2supp}
\alias{attemptstable2supp3}
\title{Translate attempts tables to hyper3 support functions}
\description{
Translate an attempts tables to a \code{hyper3} support function.
}
\usage{
attemptstable2supp3(a, decreasing=TRUE, give.supp=TRUE, dnf.last=TRUE)
}
\arguments{
  \item{a}{Data frame, see details}
  \item{decreasing}{Boolean, with \code{TRUE} meaning that the highest
      score wins [e.g. javelin distances] and \code{FALSE} meaning that the
      lowest score wins [e.g. times for a race]}
  \item{give.supp}{Boolean, return the support function or the order statistic}
  \item{dnf.last}{Boolean, should NA entries count as coming last (TRUE)
or  be ignored (FALSE)}
}
\details{

Function \code{attemptstable2supp3()} is intended for use on attempts
tables like \code{javelin_table}.

File \code{inst/javelin.Rmd} shows a use-case for
\code{attemptstable2supp3().

}

\value{Returns a \code{hyper3} object}
\author{Robin K. S. Hankin}
\seealso{\code{\link{ordertable2supp}},\code{\link{javelin}}}
\examples{

jj <- javelin_table[1:3,]
jj
attemptstable2supp3(jj)

}
