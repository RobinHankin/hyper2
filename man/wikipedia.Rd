\name{wikipedia}
\alias{wikipedia}
\alias{wikipedia_table}
\alias{wikipedia_refdesk}
\docType{data}
\title{Wikipedia refdesk usage statistics}
\description{
  Order table of questions asked in the different wikipedia refdesks,
  split by year.}
\usage{data(universities)}
\format{A \code{hyper2} object that gives a likelihood function for
  ranking of NZ universities}
\details{

  The data is taken directly from wikipedia where someone was asking
  about refdesk usage.  

\url{https://en.wikipedia.org/w/index.php?title=Wikipedia:Reference_desk/Miscellaneous&oldid=946204108}
  
The ascii source for the dataset is at \code{inst/wikipedia.txt}; each
row is a refdesk and each column a year.  Use
\code{read.table("wikipedia.txt")} to create an order table,
\code{wikipedia_table}.  Object \code{wikipedia} is the corresponding
\code{hyper2} support function.
}
\seealso{\code{\link{ordertable}}}
\examples{

data(wikipedia)
dotchart(maxp(wikipedia),log="x")
ordertable_to_ranktable(wikipedia)
}

