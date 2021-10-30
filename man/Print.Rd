\name{Print}
\alias{print.hyper2}
\alias{print}
\alias{Print}
\alias{give_warning_on_nonzero_power_sum}
\alias{nonzero_power_sum}
\alias{power_sum}
\title{
Print methods
}
\description{
Print methods for \code{hyper2} objects
}
\usage{
\method{print}{hyper2}(x, ...)
}
\arguments{
  \item{x}{An object of class \code{hyper2}}
  \item{\dots}{Further arguments, currently ignored}
}
\value{

Returns the \code{hyper2} object it was sent, invisibly.  Used mainly
for its side-effect of printing the log-likelihood function.  In the
print method, a natural logarithm is indicated with
\dQuote{\code{log()}}---not \dQuote{\code{ln()}}---consistent with R
builtin terminology \code{base::log()}.

The print method is sensitive to option
\code{give_warning_on_nonzero_power_sum}.  If \code{TRUE}, a warning is
issued if the powers have nonzero sum.  This is usually what you want
because observations are typically multinomial.  If \code{FALSE}, the
warning is suppressed.  One often needs \code{hyper2} objects with
nonzero sum as the following snippet attests:

\preformatted{
  H1 <- hyper2(list('a'),1)
  H2 <- hyper2(list('a',c('a','b')),c(1,-1))
}

Above, both objects are legitimate likelihood functions; both correspond
to the observation \dQuote{player \code{a} won once}.  But they differ
in that \code{H1} corresponds to \dQuote{player \code{a} came first in a
field of unspecified players} while \code{H2} corresponds to
\dQuote{player \code{a} came first in a field comprising \code{a} and
\code{b}}.  Sometimes you want \code{H1} but (in my experience) mostly
it is a result of coding error.  Also note the following snippet:

\preformatted{
  icons["L"] \%<>\% inc
  icons[c("L","NB")]  \%<>\% dec
}

The above two lines correspond to \dQuote{Take the \code{icons}
likelihood function and add to it the (independent) observation that a
correspondent found \sQuote{London} to be more concerning than
\sQuote{the Norfolk Broads}}.  But after the first line and before the
second, \code{icons} has a nonzero power sum, pending addition of
another term.  At this point, \code{icons} is arguably an incomplete
likelihood function as the players have not been specified; its nonzero
power sum is an indicator that it is a temporary object.  That's OK as
long as we remember to add the denominator (as carried out in the next
line) which would mean multiplying by \code{(L+NB)^{-1}}, restoring the
zero power sum.  If we forget to do this, the print method gives us a
warning which should prompt us to check the coding.

}
\author{Robin K. S. Hankin}
\examples{
data(chess)
chess
}