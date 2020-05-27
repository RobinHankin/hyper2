\name{retain}
\alias{retain}
\title{Given a wikitable or order table, retain selected competitors}
\description{
Given a wikitable or order table, sometimes we wish to retain only
certain competitors (typically the strongest).  This is not a trivial
task.
}
\usage{
retain(x, wanted, discard)
}
\arguments{
  \item{x}{order table or wikitable}
  \item{wanted,discard}{competitors to keep or discard}
}

\details{


Not sure why these differ:
\preformatted{
ordertable2supp(retain(skating_table,1:5))

log(hughes^7 * (hughes + slutskaya + kwan + cohen + suguri)^-7 *
(hughes + kwan + cohen + suguri)^-4 * (hughes + kwan + suguri)^-1 *
(hughes + cohen + suguri)^-2 * (hughes + suguri)^-2 * slutskaya^7 *
(slutskaya + kwan + cohen + suguri)^-3 * (slutskaya + cohen +
suguri)^-2 * (slutskaya + suguri)^-1 * kwan^7 * (kwan + cohen +
suguri)^-2 * cohen^6 * (cohen + suguri)^-4 * suguri)


keep(ordertable2supp(skating_table),1:5)

log(hughes^8 * (hughes + slutskaya + kwan + cohen + suguri)^-8 *
(hughes + kwan + cohen + suguri)^-4 * (hughes + kwan + suguri)^-1 *
(hughes + cohen + suguri)^-2 * (hughes + suguri)^-2 * slutskaya^8 *
(slutskaya + kwan + cohen + suguri)^-4 * (slutskaya + cohen +
suguri)^-3 * (slutskaya + suguri)^-1 * kwan^8 * (kwan + cohen +
suguri)^-2 * cohen^7 * (cohen + suguri)^-5 * suguri^-4)
}

}
\value{
Returns an order table
}
\author{Robin K. S. Hankin}
\note{
Only an imbecile would supply \code{wanted} and \code{discard}
simultaneously.
}

\seealso{\code{\link{keep}}}
\examples{
ordertable2supp(retain(skating_table,1:5))
keep(ordertable2supp(skating_table),1:5)
}
