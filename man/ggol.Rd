\name{ggol}
\alias{ggol}
\alias{choose_losers}
\alias{choose_winners}
\alias{general_grouped_order_likelihood}
\alias{order_likelihood}
\title{Order statistics}
\description{
  Various functions for calculating the likelihood function for order
statistics
}
\usage{
ggol(H, ...)
general_grouped_order_likelihood(H, ...)
choose_losers(H, all_players, losers)
choose_winners(H, all_players, winners)
elimination(all_players)
order_likelihood(M,times=1)
}

\arguments{
  \item{H}{Object of class \code{hyper2}}
  \item{\dots}{Numeric or character vectors specifying groups of
    players with  equal rank, with higher-ranking groups coming earlier
    in the argument list}
  \item{all_players,winners,losers}{Numeric or character vectors
    specifying competitors.  See details}
  \item{M}{In function \code{order_likelihood()}, a matrix with each
    row corresponding to a race.  The columns
    correspond to the finishing order; thus \code{a=M[i,j]} means that
    competitor \code{a} finished in place \code{j} in race \code{i}}
  \item{times}{Vector specifiying the number of times each row is
  observed}
}
\details{

  These functions are designed to return likelihood functions, in the
  form of lists of \code{hyper2()} objects, for typical order statistics
  such as the results of rowing heats or MasterChef tournaments.

  Function \code{ggol()} is an easily-typed alias for
  \code{general_grouped_order_likelihood()}.

  Functions \code{choose_winners()} and \code{choose_losers()} take a
  \code{hyper2} object \code{H} (a likelihood function) and return a
  list of \code{hyper2} objects.  The evaluate may be found with
  function \code{maxplist()}.


  Function elimination() gives a likelihood function for situations
  where the \emph{weakest} player is identified at each stage and
  subsequently eliminated from the competition.  It is intended for
  situations like the Great British Bake-off and Masterchef in which the
  observation is which player was chosen to leave the show.  In this
  function, argument \code{all_players} is sensitive to order, unlike
  \code{choose_winners()} and \code{choose_losers()}.  Element \code{i}
  of \code{all_players} is the \eqn{i^\mathrm{th}}{i-th} player to be
  eliminated.  Thus the first element of \code{all_players} is the first
  player to be eliminated (and would be expected to have the lowest
  strength).  The final element of \code{all_players} is the last player
  to be eliminated (or alternatively the only player not to be
  eliminated).
  

  Function \code{addrank()} is now defunct.
  
}
\author{obin K. S. Hankin}
\seealso{\code{\link{rrank}}}
\examples{

W <- hyper2(pnames=letters[1:7])
W1 <- ggol(W, 'a', letters[2:6],'g')  # 24-element list
W2 <- ggol(W, 'b', letters[3:7],'a')  # 24-element list

like_single_list(rep(1/7,6),W1)
like_series(rep(1/7,6),list(W1,W2))

# run 10 races:
r1 <- rrank(10,p=(7:1)/28)
colnames(r1) <- letters[1:7]

# Likelihood function for r1:
W <- order_likelihood(r1)

# Incorporate information from another (independent) observation:
W <- W + order_likelihood(sample(7))


H <- hyper2(pnames=letters[1:5])
H2 <- choose_losers(H,letters[1:4],letters[1:2])   # {a,b} vs {c,d}; {a,b} lost
maxplist(H2)

# Player 'a' eliminated first, then b,c,d:
maxplist(elimination(letters[1:4])) 
# Competitor 'a' has the lowest estimated strength.


# consistency check:
H <- hyper2(pnames=letters[1:5])
H <- H + order_likelihood(rrank(100,5:1)) # probs = 5/15,4/15,...,1/15
maxp(H)  # should be close to (5:1)/15




}
