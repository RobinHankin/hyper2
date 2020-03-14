\name{maxp}
\alias{maxp}
\alias{maxplist}
\alias{maxp_single}
\alias{equalp}
\alias{allequal}
\alias{equalprobs}
\title{Maximum likelihood estimation}
\description{Find the maximum likelihood estimate for p, also equal probabilities}
\usage{
maxp(H, startp=NULL, give=FALSE, fcm=NULL, fcv=NULL, SMALL=1e-6, n=10,
   show=FALSE, justlikes=FALSE, ...)
maxplist(Hlist, startp=NULL, give=FALSE, fcm=NULL, fcv=NULL, SMALL=1e-6, ...)
maxp_single(H, startp=NULL, give=FALSE, fcm=NULL, fcv=NULL, SMALL=1e-6, ...)
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
  \item{n}{Number of start points to use}
  \item{show}{Boolean, with \code{TRUE} meaning to show successive
    estimates}
  \item{justlikes}{Boolean, with \code{TRUE} meaning to return just a
      vector of estimated likelihoods}
  \item{SMALL}{Numerical minimum for probabilities}
  \item{...}{Further arguments which \code{maxp()} passes to
    \code{constrOptim()}}
}
\details{
  Function \code{maxp()} returns the maximum likelihood estimate for
  \code{p}, which has the unit sum constraint implemented.

  Function \code{maxplist()} does the same but takes a list of
  \code{hyper2} objects (for example, the output of \code{ggrl()}).
  Note that \code{maxplist()} does not have access to the gradient of
  the objective function, which makes it slow.

  If function \code{maxp()} is given a \code{suplist} object it
  dispatches to \code{maxplist()}.

  The functions do not work for the \code{masterchef_series6} likelihood
  function.  These require a bespoke optimization as shown in the
  vignette.

  Function \code{equalp()} returns the value of \eqn{p} for which all
  elements are the same.

  In functions \code{maxp()} etc, arguments \code{fcm} and \code{fcv}
  implement linear constraints to be passed to \code{constrOptim()}.
  These constraints are in addition to the usual nonnegativity
  constraints and unit-sum constraint, and are added to the \code{ui}
  and \code{ci} arguments of \code{constrOptim()} with \code{rbind()}
  and \code{c()} respectively.  The operative lines are in
  \code{maxp_single()}:

  \preformatted{
    UI <- rbind(diag(nrow = n - 1), -1, fcm)
    CI <- c(rep(SMALL, n - 1), -1 + SMALL, fcv)
  }

  where in \code{UI}, the first \eqn{n-1} rows enforce nonnegativity of
  \eqn{p_i}, \eqn{1\leq p < n}{1<=i<n}; row \eqn{n} enforces
  nonnegativity of the fillup value \eqn{p_n}; and the remaining
  (optional) rows enforce additional linear constraints.  Argument
  \code{CI} is a vector with corresponding elements.

  Examples of their use are given in the \dQuote{icons} vignette.

}
\author{Robin K. S. Hankin}
\note{

  In manpages elsewhere, \code{n=2} is used for speed reasons.  Use the
  default \code{n=10} or greater in production work.

  This functionality is peculiarly susceptible to off-by-one errors.

}
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
W1 <- ggrl(W, 'a', letters[2:3],'d')  # W1 is a suplist object
maxp(W1)

}
