\name{as.ordertable}
\alias{as.ordertable}
\title{Convert an order table with DNS entries to a nice order table}
\description{
Given an ordertable such as \code{F1_table_2017} which is a
\dQuote{wikitable} object, function \code{as.ordertable()} returns a
nicified version in which entries such as DNS are replaced with zeros.
Finishing competitors are assigned numbers \eqn{1-n} with no gaps; the
function can be used to extract a subset of competitors.
}
\usage{as.ordertable(w)}
\arguments{\item{w}{A generalized ordertable, a wikitable}}
\details{
  Operates columnwise, and treats any entry not coercable to numeric as
  DNF.
}
\value{
  Returns an ordertable suitable for coercion to a \code{hyper2} object.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{ordertable}}}
\examples{

as.ordertable(F1_table_2017)
pie(maxp(ordertable2supp(as.ordertable(F1_table_2017[1:9,]))))

}
