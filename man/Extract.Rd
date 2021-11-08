\name{Extract}
\alias{Extract.hyper2}
\alias{extract}
\alias{extractor}
\alias{assign_lowlevel}
\alias{overwrite_lowlevel}
\alias{[.hyper2}
\alias{[<-.hyper2}
\title{
Extract or replace parts of a hyper2 object
}
\description{
Extract or replace parts of a hyper2 object
}
\usage{
\method{[}{hyper2}(x, \dots)
\method{[}{hyper2}(x, index, \dots) <- value
assign_lowlevel(x,index,value)
overwrite_lowlevel(x,value)

}
\arguments{
  \item{x}{An object of class \code{hyper2}}
  \item{\dots}{Further arguments, currently ignored}
  \item{index}{A list with integer vector elements corresponding to the
      brackets whose power is to be replaced}
  \item{value}{Numeric vector of powers}
}
\details{
  These methods should work as expected, although the off-by-one issue
  might be a gotcha.

 For the extract method, \code{H[L]}, a \code{hyper2} object is
 returned.  The replace method, \code{H[L] <- value}, the index
 specifies the brackets whose powers are to be overwritten; standard
 \code{disordR} protocol is used.

 If the index argument is missing, viz \code{H1[] <- H2}, this is a
 special case.  Argument \code{H1} must be a \code{hyper2} object, and
 the idiom effectively executes \code{H1[brackets(H2)] <- powers(H2)},
 but more efficiently (note that this operation is well-defined even
 though the order of the brackets is arbitrary).  This special case is
 included in the package because it has a very natural \code{C++}
 expression [function \code{overwrite()} in the \code{src/} directory]
 that was too neat to omit.

 Altering (incrementing or decrementing) the power of a single bracket
 is possible using idiom like \code{H[x] <- H[x] + 1}; this is
 documented at \code{Ops.hyper2}, specifically
 \code{hyper2_sum_numeric()} and a discussion is given at
 \code{increment.Rd}.
 
 Functions \code{assign_lowlevel()} and \code{overwrite_lowlevel()} are
 low-level helper functions and not really intended for the end-user.

}

\value{
The extractor method returns a \code{hyper2} object, restricted to the
elements specified
}
\author{Robin K. S. Hankin}
\note{
Use \code{powers()} and \code{brackets()} to extract a numeric vector of
 powers or a list of integer vectors respectively.

Replacement idiom \code{H[x] <- val} cannot use non-trivial recycling.
This is because the elements of \code{H} are stored in an arbitrary
order, but the elements of \code{val} are stored in a particular order.
Also see function \code{hyper2_sum_numeric()}.

}
\seealso{\code{\link{hyper2}},\code{\link{Ops.hyper2}}}
\examples{
data(chess)

chess["Topalov"]
chess[c("Topalov","Anand")]
chess[c("Anand","Topalov")]

# Topalov plays Anand and wins:

chess["Topalov"] <- chess["Topalov"]+1 
chess[c("Topalov","Anand")] <- chess[c("Topalov","Anand")]-1


# Topalov plays *Kasparov* and wins:
chess["Topalov"] \%<>\% inc
chess[c("Topalov","Kasparov")] \%<>\% dec

# overwriting idiom:
H <- hyper2(list("Topalov","X"),6)
chess[] <- H

H <- icons

}
