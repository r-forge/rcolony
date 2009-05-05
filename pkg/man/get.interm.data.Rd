\name{get.interm.data}
\alias{get.interm.data}

\title{ A function to collect intermediate data produced by Colony2 while it is running. }
\description{
While it is running, Colony2 produces output to indicate it's progress. This function allows R to import the outputs produced.}

\usage{
get.interm.data(v1="CrLogL",datadir=getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v1}{The name of the variable required. }
  \item{datadir}{ ~~Describe \code{colony.object} here~~ }

}
\details{
The intermediate results available are:

\textit{Run} : The replicate run number. Variable
\textit{Tmr} : The number of temperature reductions so far within the run. Variable
\textit{Itr} : The number of iterates (reconfigurations considered) so far within the run. Variable
\textit{NSucc} : The number of successful (accepted) reconfigurations so far within the temperature. Variable
\textit{NSuccLmt} : Maximum (Limit) number of successful reconfigurations allowed within the temperature. Constant
\textit{NFail1} : The number of reconfigurations since the last update of the best likelihood within the temperature. Variable
\textit{NFail1Lmt} : Maximum (Limit) value of  NFail1 within the temperature. Constant
\textit{NFail2} : The total number of reconfigurations since the last update of the best likelihood within the run. Variable
\textit{NFail2Lmt} : Maximum (Limit) value of  NFail2 within a run. Constant. The run terminates when NFail2Lmt=NFail2 and the successful rate (see below) < 0.01
\textit{SucRate\%} : =NSucc / Itr. Variable
\textit{SucLmt\%} : =NSucc / NSuccLmt. Variable
\textit{FailLmt\%} : =NFail1 / NFail1Lmt. Variable
\textit{IterLmt\%} : = (Number of iterates) / (Maximum number of iterates) within a temperature. Variable
\textit{CrLogL} : The log likelihood of the current configuration. Variable
\textit{#F1} : Current number of paternal sib families. Variable
\textit{#F2} : Current number of maternal sib families. Variable
\textit{#F3} : Current number of sib family clusters. Variable
\textit{#FS} : Current number of full sib families. Variable
\textit{HSPair} : Current number of half-sib dyads. Variable
\textit{FSPair} : Current number of full-sib dyads. Variable
\textit{#AssgnC1} : Current number of candidate males that are assigned parentage. Variable
\textit{#AssgnC2} : Current number of candidate females that are assigned parentage. Variable
\textit{#AssgnP1} : Current number of offspring that have assigned paternity. Variable
\textit{#AssgnP2} : Current number of offspring that have assigned maternity. Variable
}
\value{
  
  }
\references{ Wang, J. (2004) Sibship reconstruction from genetic data with typing errors.  Genetics 166: 1963-1979. }
\author{Owen R. Jones}
\note{ }
\seealso{\code{\link{monitor.colony}} }
\examples{}

\keyword{ datagen }
