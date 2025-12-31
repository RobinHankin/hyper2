\name{attemptstable}
\alias{as.attemptstable}
\alias{attemptstable2supp}
\alias{attemptstable2supp3}
\title{Translate attempts tables to hyper3 support functions}
\description{
Attempts tables
}
\usage{
attemptstable2supp3(x, decreasing=TRUE, give.supp=TRUE, dnf.last=TRUE)
}
\arguments{
  \item{x}{Data frame, see details}
  \item{decreasing}{Boolean, with \code{TRUE} meaning that the highest
      score wins [e.g. javelin distances] and \code{FALSE} meaning that the
      lowest score wins [e.g. times for a race]}
  \item{give.supp}{Boolean, return the support function or the order statistic}
  \item{dnf.last}{Boolean, should NA entries count as coming last (TRUE)
or  be ignored (FALSE)}
}
\details{


An \dfn{attemptstable} object is a data frame with each row being a
competitor and each column being an attempt.  The canonical example is
\code{javelin}:

\preformatted{
> javelin_table
          throw1 throw2 throw3 throw4 throw5 throw6
Chopra     87.03  87.58  76.79      X      X  84.24
Vadlejch   83.98      X      X  82.86  86.67      X
Vesely     79.73  80.30  85.44      X  84.98      X
Weber      85.30  77.90  78.00  83.10  85.15  75.72
Nadeem     82.40      P  84.62  82.91  81.98      X
Katkavets  82.49  81.03  83.71  79.24      X      X
Mardare    81.16  81.73  82.84  81.90  83.30  81.09
Etelatalo  78.43  76.59  83.28  79.20  79.99  83.05
> 
}


The print method, by default, bolds the best attempt by each competitor.
This does not work in Rmarkdown documents; to suppress it, set option
`bold_personal_best` to `FALSE`:

```
options("bold_personal_best" = FALSE)
```

To translate an \code{attemptstable} object to a \code{hyper3} support
function, use \code{suppfun()}; function \code{attemptstable2supp3()} is
a low-level helper function.

File \code{inst/javelin.Rmd} shows a use-case for
\code{attemptstable2supp3().}
}

\value{Returns a \code{hyper3} object}
\author{Robin K. S. Hankin}
\seealso{\code{\link{ordertable2supp}},\code{\link{javelin}}}
\examples{

jj <- javelin_table[1:3,]
jj
attemptstable2supp3(jj)

}
