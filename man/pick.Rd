\name{pick}
\alias{trial}
\alias{pick}
\alias{pass}
\alias{beats}
\alias{loses}
\title{Individual pairwise trials, teams or individuals}
\description{

  A consistent suite of functions that give likelihood functions for a
  single pairwise trial between specified sets of competitors;
  supercedes \code{trial()}.

}
\usage{
pick(winners, allplayers)
pass(losers,  allplayers)
beats(winners, losers)
loses(losers, winners)
trial(winners, players, val=1)
}
\arguments{
  \item{winners}{Players on the winning team}
  \item{losers}{Players on the losing team}
  \item{allplayers}{All players, winning and losing}
  \item{players, val}{Inputs to (soon to be deprecated) function
    \code{trial()}}
}
\details{


Consider a competition between two disjoint sets of players,
\eqn{A=\left\lbrace a_1,\ldots, a_n\right\rbrace}{omitted} and
\eqn{B=\left\lbrace b_1,\ldots, b_m\right\rbrace}{omitted}.

If \eqn{A} wins the likelihood function will be
\eqn{\frac{a_1+\ldots+a_n}{a_1+\ldots+a_n+b_1+\ldots+b_m}} and if
\eqn{B} wins the likelihood function will be
\eqn{\frac{b_1+\ldots+b_m}{a_1+\ldots+a_n+b_1+\ldots+b_m}}.

We can represent a win for \eqn{A} with either \code{pick(A, c(A,B))} or
\code{beats(A,B)}; or \code{pass(B, c(A,B))} or \code{loses(B,A)}

Function \code{trial()} will be deprecated soon. It is an inefficient
synonym for \code{pick()}.

The \code{trial()} function is used in \code{inst/kka.Rmd} (but will
shortly be changed).

}
\author{Robin K. S. Hankin}
\examples{

pick(letters[1:2], letters[1:5])   # field abcde; ab beats cde
pass(letters[1:2], letters[1:5])   # field abcde; ab loses to cde

beats(letters[1:2], letters[3:5])  # field abcde; ab beats cde
loses(letters[1:2], letters[3:5])  # field abcde; ab loses to cde

monster <- "home"
players <- letters[1:4]

H <- hyper2()
H <- H + pick(c("a", "b", monster), c(players, monster))
H <- H + pick(c("a", "d", monster), c(players, monster))
H <- H + pick(c("a",      monster), c(players, monster))
H <- H + pass(c("c", "d", monster), c(players, monster))

## above see how pick() / pass() is better.

monster <- "cheating"

## Now new players e and f join the chat:
H <- H + beats(c("a", "e"), c("b", "c"))
H <- H + beats(c("a", "c", monster), c("b", "d"))
H <- H + beats(c("e", "f"), c("a", "b", monster))
H <- H + loses(c("a", "f", monster), c("d", "e"))
## above, see beats/loses are more convenient than pick/pass


H
}
