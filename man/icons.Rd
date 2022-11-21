\name{icons}
\alias{icons}
\alias{icons_matrix}
\alias{icons_table}
\alias{icons_maxp}
\alias{oneill}
\docType{data}
\title{Dataset on climate change due to O'Neill}
\description{

  Object \code{icons_matrix} is a matrix of nine rows and six columns,
  one column for each of six icons relevant to climate change.  The
  matrix entries show the number of respondents who indicated which icon
  they found most concerning.  The nine rows show different classes of
  respondents who were exposed to different subsets (of size four) of
  the six icons.

  The columns correspond to the different stimulus icons used, detailed
  below.  An extensive discussion is given in West and Hankin 2008, and
  Hankin 2010; an updated analysis is given in the \code{icons}
  vignette.

  Object \code{icons} is the corresponding likelihood function, which
  can be created with \code{saffy(icons_matrix)}.

  The object is used in \code{inst/ternaryplot_hyper2.Rmd} which shows a
  ternary plot of random samples.
  
}
\usage{data(icons)}
\details{
  The six icons were used in this study were:
  \describe{
    \item{PB}{polar bears, which face extinction through loss of ice
    floe hunting grounds}
    \item{NB}{The Norfolk Broads, which flood due to intense rainfall
    events}
    \item{L}{London flooding, as a result of sea level rise}
    \item{THC}{The Thermo-haline circulation, which may slow or stop as
      a result of anthropogenic modification of the hydrological cycle}
    \item{OA}{Oceanic acidification as a result of anthropogenic emissions
    of carbon dioxide}
    \item{WAIS}{The West Antarctic Ice Sheet, which is calving into the sea
    as a result of climate change}
  }
}
\source{
  Data kindly supplied by Saffron O'Neill of the University of East Anglia
}
\references{
  \itemize{
    \item S. J. O'Neill and M. Hulme 2009. \emph{An iconic approach for
    representing climate change}.  Global Environmental Change,
    19:402-410
    \item I. Lorenzoni and N. Pidgeon 2005. \emph{Defining Dangers of
      Climate Change and Individual Behaviour: Closing the Gap}. 
    In  \emph{Avoiding Dangerous Climate Change} (conference
    proceedings), UK Met Office, Exeter, 1-3 February
    \item R. K. S. Hankin 2010. \dQuote{A generalization of the Dirichlet
      distribution}.   \emph{Journal of Statistical software}, 33:11
  }
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{matrix2supp}}}
\examples{
data(icons)
pie(icons_maxp)
equalp.test(icons)

}
\keyword{datasets}
