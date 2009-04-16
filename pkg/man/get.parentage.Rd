\name{get.parentage}
\alias{get.parentage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A funciton to extract the paternity and maternity information from the colony object created with get.colony.data(). }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.parentage(colony.object, pairwise = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colony.object}{A list created by \code{get.colony.data}.}
  \item{pairwise}{TRUE/FALSE: should the parentage information be derived from the pairwise likelihood estimates (TRUE), of from the full likelihood methods (FALSE). See Wang et al. for details.}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  A data frame with 3 columns. (1) mums, (2) dads, (3) frequency. 
}
\references{ Wang }
\author{Owen R. Jones}
\note{ 

}

\seealso{ \code{\link{get.colony.data}} }

\examples{

data(testdata)
parents<-get.parentage(testdata)


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
