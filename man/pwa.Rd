\name{pwa}
\alias{pwa}
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

}
\usage{pwa(H, pwa, chameleon = "S")}
\arguments{
  \item{H}{A hyper2 object}
  \item{pwa}{A list of the players with the supposed advantage; may be
    character in the case of a named hyper2 object, or an integer
    vector}
  \item{chameleon}{String representing the advantage}
}

\details{

Given an object of class \code{hyper2} and a competitor \code{a}, we
replace every occurence of \code{a} with \code{a+S}, with \code{S}
representing the extra strength conferred.

However, the function also takes a vector of competitors.  If there is
more than one competitor, the resulting likelihood function does not
seem to instantiate any simple situation.  

Nice examples of \code{pwa()} are given in \file{inst/cook.Rmd} and
\file{inst/universities.Rmd}.


}
\value{Returns an object of class \code{hyper2}.}
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

H <- ordervec2supp(c(a = 2, b = 3, c = 1, d = 5, e = 4))
pwa(H,'a')


formula1 \%>\% pwa("Hamilton","pole")


## Three five-way races:
H1 <- ordervec2supp(c(a = 1, b = 5, c = 3, d = 4, e = 2))
H2 <- ordervec2supp(c(a = 2, b = 3, c = 1, d = 4, e = 5))
H3 <- ordervec2supp(c(a = 4, b = 3, c = 0, d = 1, e = 2))

## Now it is revealed that a,b,c had some advantage in races 1,2,3 respectively:
Ht <- pwa(H1,'a') + pwa(H2,'b') + pwa(H2,'c')

specificp.test(Ht,"S")

}
