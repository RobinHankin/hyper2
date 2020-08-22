\name{heptathlon}
\alias{heptathlon}
\alias{heptathlon_table}
\alias{heptathlon_ordertable}
\docType{data}
\title{Heptathlon dataset}
\description{Results from the 2019 World Athletics Championships,
  Women's heptathlon}
\usage{data(heptathlon)}
\format{An order table and a \code{hyper2} object that gives a support function}
\details{

Object \code{heptathlon} is a \code{hyper2} object that gives a
likelihood function for the strengths of the finishing competitors of
the Women's heptathlon at the 2019 World Athletics Championships.
Object \code{heptathlon_ordertable} is an order table: a data frame with
rows being competitors, columns being events, and entries being being
places.  Thus looking at the first row, first column we see that Annie
Kunz placed seventh.

Object \code{heptathlon_ordertable} is not simply
\code{read.table(inst/heptathlon.txt)} because four non-finishers were
removed from the table.  The appropriate idiom is

\preformatted{
  a <- read.table("inst/heptathlon.txt")
  heptathlon_ordertable <-
       apply(a,2,function(x){`[<-`(x,order(x),seq_along(x))})
}

and \code{heptathlon} is then
\code{ordertable2supp(heptathlon_ordertable)}.

}
\references{

Wikipedia contributors, 2020.  \dQuote{2019 World Athletics
Championships - Women's heptathlon}. {Wikipedia}{,} The Free
Encyclopedia,
\url{https://en.wikipedia.org/w/index.php?title=2019_World_Athletics_Championships_\%E2\%80\%93_Women\%27s_heptathlon&oldid=932628752},
\emph{Online; accessed 5-March-2020}

  }
\seealso{\code{\link{ordertable2supp}}}
\examples{
data(heptathlon)
dotchart(maxp(heptathlon,n=2))  #n=2 specified here for speed
}
