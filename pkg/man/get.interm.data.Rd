\name{get.interm.data}
\alias{get.interm.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A funciton to collect intermediate data produced by Colony2 while it is running. }
\description{
While it is running, Colony2 produces output to indicate it's progress. }
\usage{
get.interm.data(v1="CrLogL",datadir=getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colony.object}{ ~~Describe \code{colony.object} here~~ }
  \item{maintitle}{ ~~Describe \code{maintitle} here~~ }
  \item{prob}{ ~~Describe \code{prob} here~~ }
  \item{pairwise}{ ~~Describe \code{pairwise} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as

get.interm.data<-function(v1="CrLogL",datadir=getwd()){

getval<-function(v1,xx){

ind<-grep(paste(v1,"=",sep=""),xx)
s1<-"[A-Za-z =0-9,]*"
s2<-"[ ]*([-.+E0-9]+),[A-Z#a-z =0-9,]*"
s3<-paste(s1,paste(v1,"=",sep=""),s2,sep="")
x.out<-NULL

for (i in 1:length(ind)){
	x.out[i]<-as.numeric(sub(s3,"\\1",xx[ind][i]))
	}

return(x.out)}

outfile<-readLines(paste(datadir,"temp.txt",sep=""))

Itr<-getval("Itr",outfile)
CrLogL<-getval(v1,outfile)
chk<-c(length(Itr),length(CrLogL))

df1<-data.frame(Itr=Itr[1:min(chk)],CrLogL=CrLogL[1:min(chk)])
return(df1)
}



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
