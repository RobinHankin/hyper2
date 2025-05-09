\name{maxp}
\alias{maxp}
\alias{maxplist}
\alias{maxp_lsl}
\alias{maxp_single}
\alias{maxp_single2}
\alias{maxp_simplex}
\alias{equalp}
\alias{allequal}
\alias{equalprobs}
\title{Maximum likelihood estimation}
\description{
  Find the maximum likelihood estimate for p, also equal probabilities}
\usage{
maxp(H, startp=NULL, give=FALSE, fcm=NULL, fcv=NULL, SMALL=1e-6, n=1,
   show=FALSE, justlikes=FALSE, ...)
maxplist(Hlist, startp=NULL, give=FALSE, fcm=NULL, fcv=NULL, SMALL=1e-6, ...)
maxp_single(H, startp=NULL, give=FALSE, fcm=NULL, fcv=NULL, SMALL=1e-6,
   maxtry=100, ...)
maxp_single2(H, startp=NULL, give=FALSE, fcm=NULL, fcv=NULL, SMALL=1e-6,
   maxtry=100, ...)
maxp_simplex(H, n=100, show=FALSE, give=FALSE, ...)
maxp_lsl(HLSL, startp = NULL, give = FALSE, fcm = NULL, fcv = NULL, SMALL=1e-6, ...)
equalp(H)
}
\arguments{
  \item{H}{A \code{hyper2} or \code{hyper3} object}
  \item{Hlist}{A list with elements all \code{hyper2} objects}
  \item{HLSL}{An \code{lsl} object}
  \item{startp}{A vector of probabilities specifying the start-point for
    optimization; if a full unit-sum vector, then the fill-up value will
    be removed by \code{indep()} (except for \code{maxp_lsl()})}
  \item{give}{Boolean, with default \code{FALSE} meaning to return just
    the evaluate (including fillup), and \code{TRUE} meaning to return
    the entire formal output of the optimization routine.  In function
    \code{maxp()}, a non-logical value [such as \code{0}] gives only the
    two most important pieces of information, viz the evaluate and a
    log-likelihood}
  \item{fcm,fcv}{Further problem-specific constraints}
  \item{n}{Number of start points to use}
  \item{show}{Boolean, with \code{TRUE} meaning to show successive
    estimates}
  \item{justlikes}{Boolean, with \code{TRUE} meaning to return just a
    vector of estimated likelihoods}
  \item{SMALL}{Numerical minimum for probabilities}
  \item{maxtry}{Integer specifying maximum number of times to try
    \code{constrOptim()} with slightly differing start points, to avoid
    a known \R bug which reports \code{wmmin is not finite}, bugzilla
    id 17703}
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

  Functions \code{maxp_single()} and \code{maxp_single2()} are helper
  functions which perform a single constrained optimization using
  \code{base::constrOptim()} or \code{alabama::constrOptim.nl()}
  respectively.  The functions should produce identical (or at least
  very similar) results.  They are used by \code{maxp()} and
  \code{maxp_simplex()} which dispatch to either \code{maxp_single()} or
  \code{maxp_single2()} depending on the value of option
  \code{use_alabama}.  If \code{TRUE}, they will use (experimental)
  \code{maxp_single2()}, otherwise (default) \code{maxp_single()}.
  Function \code{maxp_single()} is prone to the \dQuote{wmmin not
  finite} bug [bugzilla id 17703] but on the other hand is a bit
  slower.  I am not sure which one is better at this time.
 
  Function \code{maxp_simplex()} is intended for complicated or flat
  likelihood functions where finding local maxima might be a problem.
  It repeatedly calls \code{maxp_single()}, starting from a different
  randomly chosen point in the simplex each time.  This function does
  not take \code{fcm} or \code{fcv} arguments, it operates over the
  whole simplex (hence the name).  Further arguments, \code{...}, are
  passed to \code{maxp_single()}.
  
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

  In manpages elsewhere, \code{n=2} is sometimes used.  Previous
  advice was to use \code{n=10} or greater in production work, but I
  now think this is overly cautious and \code{n=1} is perfectly
  adequate unless the dimension of the problem is large.

  The (bordered) Hessian is given by function \code{hessian()},
  documented at \code{gradient.Rd}; use this to assess the
  \dQuote{sharpness} of the maximum.

  Function \code{maxp()} takes \code{hyper2} or \code{hyper3} objects
  but it does not currently work with \code{lsl} objects; use
  \code{maxp_lsl()}.

  The built-in datasets generally include a pre-calculated result of
  running \code{maxp()}; thus hyper2 object \code{icons} and
  \code{icons_maxp} are included in the same \code{.rda} file.
  
  Function \code{maxp()} can trigger a known \R bug (bugzilla id 17703)
  which reports \dQuote{\code{wmmin is not finite}}.  Setting option
  \code{use_alabama} to \code{TRUE} makes the package use a different
  optimization routine.

}
\seealso{\code{\link{gradient}},\code{\link{fillup}}}
\examples{

maxp(icons)

W <- hyper2(pnames=letters[1:5])
W1 <- ggrl(W, 'a', letters[2:3],'d')  # W1 is a suplist object
\dontrun{maxp(W1)}  # takes a long time to maximize a suplist

}
