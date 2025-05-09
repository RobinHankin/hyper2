\name{Print}
\alias{print.hyper2}
\alias{print.hyper3}
\alias{print}
\alias{Print}
\alias{pnv}
\alias{give_warning_on_nonzero_power_sum}
\alias{nonzero_power_sum}
\alias{power_sum}
\title{Print methods}
\description{
Print methods for \code{hyper2} and \code{hyper3} objects
}
\usage{
\method{print}{hyper2}(x, ...)
\method{print}{hyper3}(x, ...)
}
\arguments{
  \item{x}{An object of class \code{hyper2} or \code{hyper3}}
  \item{\dots}{Further arguments, currently ignored}
}
\value{
  Returns the \code{hyper2} or \code{hyper3} object it was sent,
  invisibly.

  Function \code{pnv()} (\dQuote{\bold{p}rint \bold{n}amed
  \bold{v}ector}) takes a named vector and returns a character string
  that is used in the \code{hyper3} print method.  It is sensitive to
  base R print options such as \code{digits} and \code{scipen}.
  Currently there is no space around the \dQuote{=} symbol but this is
  easy to change.
  
}
\details{

Used mainly for their side-effect of printing the log-likelihood
function.  In the print method, a natural logarithm is indicated with
\dQuote{\code{log()}}---not \dQuote{\code{ln()}}---consistent with \R
builtin terminology \code{base::log()}.

The \code{hyper2} print method is sensitive to option
\code{give_warning_on_nonzero_power_sum}.  If \code{TRUE}, a warning is
issued if the powers have nonzero sum.  This is usually what you want
because observations are typically multinomial; a warning indicates
nonzero sum of powers, which should prompt us to check the coding.
Vignette \code{zeropower} gives a discussion of this issue.

}
\author{Robin K. S. Hankin}
\note{
  Sometimes the use of \code{pnv()} can be confusing, as distinct
  brackets can appear to be identical, as per the example.  See how the
  two terms with power \eqn{-1} appear to be identical but actually
  differ by \code{1e-12}, invisible to the print method which only shows
  seven significant figures.
}
\examples{
data(chess)
chess

getOption("digits")
hyper3(list(c(a=1), c(a=1, b=pi),c(a=1, b=pi+1e-12)), powers = c(2, -1, -1))

}
