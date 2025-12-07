\name{pwa}
\alias{pwa}
\alias{pwa3}
\alias{pwa3_single}
\alias{chameleon}
\alias{extra_strength}
\alias{additional_strength}
\alias{training_strength}
\alias{wet_strength}
\alias{white_strength}
\title{Player with advantage}
\description{

Commonly, when considering competitive situations we suspect that one
player has an advantage of some type which we would like to quantify in
terms of an additional strength.  Examples might include racing at pole
position, playing white in chess, or playing soccer at one's home
ground.  Function \code{pwa()} (\dQuote{player with advantage}) returns
a modified \code{hyper2} object with the additional strength represented
as a reified entity.

Function \code{pwa3()} is the \code{hyper3} equivalent; it modifies the
weight of specified players.  Function \code{pwa3_single()} is a
lower-level helper function that takes a single player name and its
lambda value as second and third arguments.  The named argument
mechanism is the same as that of \code{subs()} in the \CRANpkg{mvp}
package.

}
\usage{
pwa(H, pwa, chameleon = "S")
pwa3_single(H3, pwa, lambda)
pwa3(H3, ...)
}

\arguments{
  \item{H}{A hyper2 object}
  \item{H3}{A hyper3 object}
  \item{pwa}{A list of the players with the supposed advantage; may be
    character in the case of a named hyper2 object, or an integer
    vector}
  \item{chameleon}{String representing the advantage}
  \item{lambda}{Value of person with advantage}
  \item{...}{Further named arguments; see details}
}

\details{

Given \code{H} of class \code{hyper2} and a competitor \code{a},
\code{pwa(H,"a")} replaces every occurrence of \code{a} in \code{H} with
\code{a+S}, with \code{S} representing the extra strength conferred.
However, \code{a} may be a vector of competitors.  If \code{a} includes
more than one competitor, the resulting likelihood function does not
seem to instantiate any simple situation.

Nice examples of \code{pwa()} are given in \file{inst/cook.Rmd} and
\file{inst/universities.Rmd}, and some discussion of \code{pwa3()} is
given in \code{inst/pwa3.Rmd}.

}
\value{Returns an object of class \code{hyper2} or \code{hyper3}.}
\author{Robin K. S. Hankin}
\note{

Earlier versions of this package gave a contrived sequence of
observations, presented as an example of \code{pwa()} with multiple
advantaged competitors.  I removed it because the logic was flawed, but
it featured a chameleon who could impersonate (and indeed eat) certain
competitors, which is why the third argument is so named.

The aliases commemorate some uses of the function in the vignettes and
markdown files in the \file{inst/} directory.

}

\seealso{\code{\link{ordervec2supp}}}
\examples{

summary(formula1 |> pwa("Hamilton","pole"))

H <- ordervec2supp(c(a = 2, b = 3, c = 1, d = 5, e = 4))
pwa(H,'a')
pwa3_single(H, "a", 1.888)

pwa3(H, b=1.8, c=0.7)

## Four races between a,b,c,d:
H1 <- ordervec2supp(c(a = 1, b = 3, c = 4, d = 2))
H2 <- ordervec2supp(c(a = 0, b = 1, c = 3, d = 2))
H3 <- ordervec2supp(c(a = 4, b = 2, c = 1, d = 3))
H4 <- ordervec2supp(c(a = 3, b = 4, c = 1, d = 2))

## Now it is revealed that a,b,c had some advantage in races 1,2,3
## respectively.  Is there evidence that this advantage exists?

\dontrun{  # takes ~10 seconds, too long for here
specificp.test(pwa(H1,'a') + pwa(H2,'b') + pwa(H3,'c') + H4,"S")
}


}
