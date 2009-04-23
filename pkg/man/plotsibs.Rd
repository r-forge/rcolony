\name{plotsibs}
\alias{plotsibs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A function to plot the sibships (full and half-sibs) in an x-y style plot. }
\description{
This function plots the estimated sibships from Colony data. Both the x and y axis represent individuals in the population and points indicate a sibship assignment between individuals x and y.
\usage{
plotsibs(colony.object, maintitle = "sibships", prob = 0.8, pairwise = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colony.object}{ A list object derived from the colony data (\code{\link{get.colony.data}}). }
  \item{maintitle}{ The plot's title. }
  \item{prob}{ Assignment probability threshold. The probability of sibship must be greater than this value to be included in the plot.}
  \item{pairwise}{ TRUE/FALSE: should pairwise sibship estimates be used. (TRUE= pairwise, FALSE=full likelihood) }
}
\details{
Full sibships are given in the upper part of the plot, and half-sibships are given in the lower part. The data can be derived from pairwise methods, or from the full likelihood method.}
}
\value{
  A plot showing sibships.
}
\references{ ~put references to the literature/web site here ~ }
\author{Owen R. Jones}
\note{
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data(testdata)
plotsibs(testdata,maintitle="Sibships (paiwise)",pairwise=TRUE)


plotsibs(testdata,maintitle="Sibships (full)",pairwise=FALSE)



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
