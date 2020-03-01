\name{suplist}
\alias{suplist}
\alias{Ops.suplist}
\alias{suplist_add}
\alias{as.suplist}
\alias{sum.suplist}
\title{Methods for suplist objects}
\description{

  Basic functionality for lists of \code{hyper2} objects, allowing the
  user to concatenate independent observations which are themselves
  composite objects such as returned by \code{ggrl()}.

}
\usage{
\method{Ops}{suplist}(e1, e2)
\method{sum}{suplist}(x,...,na.rm=FALSE)
suplist_add(e1,e2)
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
  objects, then a likelihood function for \code{p} would be
  \eqn{\mbox{P}(p,H_1)+\cdots+\mbox{P}(p,H_n)}{P(p,H1)+...+P(p,Hn)}.
  This is because the elements of a \code{suplist} object are disjoint
  alternatives.
  
  Functions \code{suplist_add()} and \code{sum.suplist()} implement
 \dQuote{\code{W1+W2}} as the likelihood function for independent
 observations \code{W1} and \code{W2}.

  If \code{S1=list(H1,...,Hr)} and \code{S2=list(I1,...,Is)} where
  \code{Hx,Ix} are \code{hyper2} objects, then the likelihood function
  for \dQuote{\code{S1+S2}} is the likelihood function for \code{S1}
  followed by (independent) \code{S2}.  Formally

  \deqn{
    \mbox{P}(p;S_1+S_2) =
    \left(
    \mbox{P}(I_1|p)
    +\cdots+
    \mbox{P}(I_s|p)
    \right)\cdot\left(
    \mbox{P}(I_1|p)
    +\cdots+
    \mbox{P}(I_s|p)
    \right)}{omitted; see PDF}
  
  However, \code{W1+W2} is typically a large and unwieldy object, and
  can be very slow to evaluate.  These functions are here here because
  they provide slick R idiom.

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

# likelihood function for indendent observations  W1,W2,W3:

W1+W2+W3 # 24-element list


dotchart(maxplist(W1+W1+W3),pch=16)

## Following shows two ways to evaluate the same thing:

p <- indep(equalp(W1[[1]]))
like_single_list(p,W1)*like_single_list(p,W2)*like_single_list(p,W3)
like_single_list(p,W1+W2+W3)

## Note that the first is *much* faster but the second is nicer R idiom




}
\keyword{symbolmath}
