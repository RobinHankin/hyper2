\name{suplist}
\alias{suplist}
\alias{supplist}
\alias{Ops.suplist}
\alias{suplist_add}
\alias{as.suplist}
\alias{sum.suplist}
\alias{suplist_times_scalar}
\alias{lsl}
\alias{loglik_lsl}
\alias{Ops.lsl}
\alias{lsl_add}
\title{Methods for suplist objects}
\description{\loadmathjax

  Basic functionality for lists of \code{hyper2} objects, allowing the
  user to concatenate independent observations which are themselves
  composite objects such as returned by \code{ggrl()}.

}
\usage{
\method{Ops}{suplist}(e1, e2)
\method{sum}{suplist}(x,...,na.rm=FALSE)
suplist_add(e1,e2)
suplist_times_scalar(e1,e2)
as.suplist(L)
}
\arguments{
  \item{e1,e2}{Objects of class \code{suplist}, here interpreted as
    a list of possible likelihood functions (who should be added)}
  \item{x,...,na.rm}{In the \code{sum()} method, objects to be summed;
    \code{na.rm} is currently ignored}
  \item{L}{A list of \code{hyper2} objects}
}
\details{

  A \code{suplist} object is a list of \code{hyper2} objects.  Each
  element is a \code{hyper2} object that is consistent with an
  incomplete rank observation \eqn{R}; the list elements are exclusive
  and exhaustive for \eqn{R}.  If \code{S} is a \code{suplist} object,
  and \code{S=list(H1,H2,...,Hn)} where the \code{Hi} are \code{hyper2}
  objects, then
  \mjeqn{\mbox{Prob}(p|H_1)+\cdots+\mbox{Prob}(p|H_n)}{Prob(p|H1)+...+Prob(p|Hn)}.
  This is because the elements of a \code{suplist} object are disjoint
  alternatives.

 It is \strong{incorrect} to say that a likelihood function
  \mjeqn{\mathcal{L}_S(p)}{L_S(p)} for \eqn{p} is the sum of separate
  likelihood functions.  This is incorrect because the arbitrary
  multiplicative constant messes up the math, for example we might have
  \mjeqn{\mathcal{L}_{H_1}(p)=C_1\mathrm{Prob}(p|H_1)}{?} and
  \mjeqn{\mathcal{L}_{H_2}(p)=C_2\mathrm{Prob}(p|H_2)}{?} and indeed
  \mjeqn{\mathcal{L}_{{H_1}\cup H_2}(p)=C_{12}\left(\mathrm{Prob}(p|H_1)+\mathrm{Prob}(p|H_2)\right)}{?}  but

 \mjdeqn{\mathcal{L}_{H_1}(p)+\mathcal{L}_{H_2}(p) \neq
   C_1\mathrm{Prob}(p|H_1)+C_2\mathrm{Prob}(p|H_2)}{see PDF}
    
 (the right hand side is meaningless).

 Functions \code{suplist_add()} and \code{sum.suplist()} implement
  \dQuote{\code{S1+S2}} as the support function for independent
  observations \code{S1} and \code{S2}.  The idea is that the support
  functions \dQuote{add} in the following sense.  If
  \code{S1=list(H1,...,Hr)} and \code{S2=list(I1,...,Is)} where
  \code{Hx,Ix} are \code{hyper2} objects, then the likelihood function
  for \dQuote{\code{S1+S2}} is the likelihood function for \code{S1}
  followed by (independent) \code{S2}.  Formally

  \mjdeqn{
    \mbox{Prob}(p|S_1+S_2) =
    \left(
    \mbox{Prob}(p|H_1)
    +\cdots+
    \mbox{Prob}(p|H_r)
    \right)\cdot\left(
    \mbox{Prob}(p|I_1)
    +\cdots+
    \mbox{Prob}(p|I_s)
    \right)}{omitted; see PDF}

  \mjdeqn{
    \log\mbox{Prob}(p|S_1+S_2) =
    \log\left(
    \mbox{Prob}(p|H_1)
    +\cdots+
    \mbox{Prob}(p|H_r)
    \right)+\log\left(
    \mbox{Prob}(p|I_1)
    +\cdots+
    \mbox{Prob}(p|I_s)
    \right)}{omitted; see PDF}
  
  However, \code{S1+S2} is typically a large and unwieldy object, and
  can be very slow to evaluate.  These functions are here because they
  provide slick R idiom.

  The experimental \code{lsl} mechanism furnishes an alternative
  methodology which is more computationally efficient at the expense of
  a non-expicit likelihood function.  It is not clear at present (2022)
  which of the two systems is better.
  
}
\value{
Returns a \code{suplist} object.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{Ops.hyper2}},\code{\link{Extract}},\code{\link{loglik}}}
\examples{

W <- hyper2(pnames=letters[1:5])
W1 <- ggrl(W, 'a', letters[2:3],'d')  # 2-element list
W2 <- ggrl(W, 'e', letters[1:3],'d')  # 6-element list
W3 <- ggrl(W, 'c', letters[4:5],'a')  # 2-element list

# likelihood function for independent observations  W1,W2,W3:

W1+W2+W3 # A 2*6*2=24-element list

like_single_list(equalp(W),W1+W2+W3)
\dontrun{dotchart(maxplist(W1+W1+W3),pch=16)} # takes a long time

a <- lsl(list(W1,W2,W3),4:6)  # observe W1 four times, W2 five times and W3 six times
loglik_lsl(equalp(W),a,log=TRUE)

}
\keyword{symbolmath}
