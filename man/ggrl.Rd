\name{ggol}
\alias{ggol}
\alias{ggrl}
\alias{goodbad}
\alias{choose_losers}
\alias{choose_winners}
\alias{general_grouped_rank_likelihood}
\alias{general_grouped_order_likelihood}
\alias{rank_likelihood}
\alias{rankvec_likelihood}
\alias{elimination}
\alias{race}
\alias{Plackett}
\alias{Plackett-Luce}
\title{Order statistics}
\description{
  Various functions for calculating the likelihood function for order
statistics
}
\usage{
ggrl(H, ...)
general_grouped_rank_likelihood(H, ...)
goodbad(winners,losers)
elimination(all_players)
rankvec_likelihood(v,nonfinishers)
race(v,nonfinishers)
}

\arguments{
  \item{H}{Object of class \code{hyper2}}
  \item{\dots}{Numeric or character vectors specifying groups of
    players with  equal rank, with higher-ranking groups coming earlier
    in the argument list}
  \item{all_players,winners,losers}{Numeric or character vectors
    specifying competitors.  See details}
  \item{v}{A character vector specifying ranks.  Thus
  \code{c("b","c","a")} means that \code{b} came first, \code{c} second,
  and \code{a} third} 
\item{nonfinishers}{A character vector with entries corresponding to
  competitors who did not finish.  Thus
  \code{race(c("a","b"),c("p","q"))} means that the field is
  \code{a,b,c,d}; \code{a} came first, \code{b} came second and \code{c}
  and \code{d} did not finish}
}
\details{

  These functions are designed to return likelihood functions, in the
  form of lists of \code{hyper2()} objects, for typical order statistics
  such as the results of rowing heats or MasterChef ments.

  Direct use of \code{rankvec_likelihood()} is discouraged: use
  \code{suppfun()} instead, for example \code{suppfun(letters)}.
  
  Function \code{ggrl()} is an easily-typed alias for
  \code{general_grouped_rank_likelihood()}.

  Function \code{goodbad()} is a convenience function for \code{ggrl()}
  in which a bunch of contestants is judged.  It returns a likelihood
  function for the observation that the members of one subset were
  better than those of another.  Thus
  \code{goodbad(letters[1:3],letters[4:5])} corresponds to the
  observation that \code{d} and \code{e} were put into an elimination
  trial (and \code{abc} were not).
  
  Function \code{elimination()} gives a likelihood function for situations
  where the \emph{weakest} player is identified at each stage and
  subsequently eliminated from the competition.  It is intended for
  situations like the Great British Bake-off and Masterchef in which the
  observation is which player was chosen to leave the show.  In this
  function, argument \code{all_players} is sensitive to order, unlike
  \code{choose_winners()} and \code{choose_losers()} (an integer
  \code{n} is interpreted as \code{letters[seq_len(n)]}).  Element
  \code{i} of \code{all_players} is the \eqn{i^\mathrm{th}}{i-th} player
  to be eliminated.  Thus the first element of \code{all_players} is the
  first player to be eliminated (and would be expected to have the
  lowest strength).  The final element of \code{all_players} is the last
  player to be eliminated (or alternatively the only player not to be
  eliminated).

  Function \code{rank_likelihood()} is deprecated: use [\code{S3}
  generic] \code{supp.ranktable()} instead.  This takes a character
  vector of competitors with the order of elements corresponding to the
  finishing order; a Plackett-Luce likelihood function is returned.
  Thus \code{v=c("d","b","c","a")} corresponds to \code{d} coming first,
  \code{b} second, \code{c} third, and \code{a} fourth.  Function
  \code{race()} is an arguably more memorable synonym.

  An example of \code{race()} is given in \code{inst/rowing.Rmd}, and
  examples of \code{ggrl()} are given in \code{inst/loser.Rmd} and
  \code{inst/masterchef.Rmd}.

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{rrank}},\code{\link{ordertable2supp}},\code{\link{race3}}}
\examples{

W <- hyper2(pnames=letters[1:5])
W1 <- ggrl(W, 'a', letters[2:4],'e')  # 6-element list
W2 <- ggrl(W, 'b', letters[3:5],'a')  # 6-element list

like_single_list(equalp(W1),W1)
like_series(equalp(W1),list(W1,W2))

if(FALSE){  # takes too long
# run 10 races:
r1 <- rrank(10,p=(7:1)/28)
colnames(r1) <- letters[1:7]

# Likelihood function for r1:
suppfun(r1)

# convert a rank table to a support function:
suppfun(wikitable_to_ranktable(volvo_table))

H <- hyper2()
for(i in 1:20){
  H <- H + race(sample(letters[1:5],sample(3,1),replace=FALSE))
}
equalp.test(H) # should not be significant (null is true)

H1 <- hyper2(pnames=letters[1:5])
H2 <- choose_losers(H1,letters[1:4],letters[1:2])   # {a,b} vs {c,d}; {a,b} lost
maxplist(H2,control=list(maxit=1))  # control set to save time
}

}
