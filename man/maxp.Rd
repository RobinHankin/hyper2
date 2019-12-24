\name{maxp}
\alias{maxp}
\alias{maxplist}
\alias{equalp}
\alias{allequal}
\alias{equalprobs}
\title{Maximum likelihood estimation}
\description{Find the maximum likelihood estimate for p, also equal probabilities}
\usage{
maxp(H, startp=NULL, give=FALSE, fcm=NULL, fcv=NULL, SMALL=1e-5, ...)
maxplist(Hlist, startp=NULL, give=FALSE, fcm=NULL, fcv=NULL, SMALL=1e-5, ...)
equalp(H)
}
\arguments{
  \item{H}{A \code{hyper2} object}
  \item{Hlist}{A list with elements all \code{hyper2} objects}
  \item{startp}{A vector of probabilities}
  \item{give}{Boolean, with default \code{FALSE} meaning to return just
    the evaluate (including fillup), and \code{TRUE} meaning to return
    the entire formal output of the optimization routine}
  \item{fcm,fcv}{Further problem-specific constraints}
  \item{SMALL}{Numerical minimum for probabilities}
  \item{...}{Further arguments which \code{maxp()} passes to
    \code{constrOptim()}}
}
\details{
  Function \code{maxp()} returns the maximum likelihood estimate for
  \code{p}, which has the unit sum constraint implemented.

  Function \code{maxplist()} does the same but takes a list of
  \code{hyper2} objects (for example, the output of \code{ggol()}).

  If function \code{maxp()} is given a \code{suplist} object it
  dispatches to \code{maxplist()}.

  The functions do not work for the \code{masterchef_series6} likelihood
  function.  These require a bespoke optimization as shown in the
  vignette.

  Function \code{equalp()} returns the value of \eqn{p} for which all
  elements are the same.

  In functions \code{maxp()} and \code{maxplike()}, arguments \code{fcm}
  and \code{fcv} implement linear constraints to be passed to
  \code{constrOptim()}.  These constraints are in addition to the usual
  nonnegativity constraints and unit-sum constraint.  Examples of their
  use are given in the \dQuote{icons} vignette.
  

}
\author{Robin K. S. Hankin}
\note{This functionality is peculiarly susceptible to off-by-one errors.}
\seealso{\code{\link{gradient}},\code{\link{fillup}}}
\examples{

data(chess)
maxp(chess)


data(rowing)

(x1 <- loglik(indep(maxp  (sculls2016)),sculls2016))
(x2 <- loglik(indep(equalp(sculls2016)),sculls2016))



## get a p-value for the null of equal player strengths:
pchisq(2*(x1-x2),df=size(sculls2016)-1,lower.tail=FALSE)

## Note that Wilks's theorem is only asymptotic.


W <- hyper2(pnames=letters[1:5])
W1 <- ggol(W, 'a', letters[2:3],'d')  # W1 is a suplist object
maxp(W1)

}
