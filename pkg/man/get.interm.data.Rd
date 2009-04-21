\name{plotsibs}
\alias{plotsibs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotsibs(colony.object, maintitle = "sibships", prob = 0.8, pairwise = FALSE)
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
function(colony.object,maintitle="sibships",prob=0.8,pairwise=FALSE){
	 
	 #Get full and half sibship data.
if(pairwise==FALSE){	
sibs<-colony.object$sibs}else{
sibs<-colony.object$pairwise.sibs
sibs$Probability<-rep(1,dim(sibs)[1])}

	
alloffspring<-colony.object$offspring	
alloffspring$offspring.numID<-as.numeric(alloffspring$offspring.numID)

	 #Apply the numeric ID to the sibship data.
	 sibs<-merge(sibs,alloffspring,by.x="OffspringID1",by.y="offspringID",sort=FALSE)
	 names(sibs)[names(sibs)=="offspring.numID"]<-"OffspringID1.num"
	 
	 sibs<-merge(sibs,alloffspring,by.x="OffspringID2",by.y="offspringID",sort=FALSE)
	 names(sibs)[names(sibs)=="offspring.numID"]<-"OffspringID2.num"
	 sibs$OffspringID1.num<-as.numeric(sibs$OffspringID1.num)
	 sibs$OffspringID2.num<-as.numeric(sibs$OffspringID2.num)
	 
	 
	 #Set up the plot area.
	 plot(sibs$OffspringID1.num,sibs$OffspringID2.num,type="n",axes=FALSE,xlab="Offspring X",ylab="Offspring Y")
	 axis(1,at=alloffspring$offspring.numID,labels=alloffspring$offspringID,las=2)
	 axis(2,at=alloffspring$offspring.numID,labels=alloffspring$offspringID,las=2)
	 
	 polygon(c(0,1.05*max(alloffspring$offspring.numID),1.05*max(alloffspring$offspring.numID)),c(0,0,1.05*max(alloffspring$offspring.numID)),col="#FF669930",border=NA)
	 polygon(c(0,0,1.05*max(alloffspring$offspring.numID)),c(0,1.05*max(alloffspring$offspring.numID),1.05*max(alloffspring$offspring.numID)),col="#33CCFF30",border=NA)
	 
	 legend("topleft","Full Sib",bty="n")
	 legend("bottomright","Half Sib",bty="n")
	 
	 title(main=maintitle)
	 
	 #Plot the data.
	 hs<-subset(sibs,type=="Half"&Probability>prob)
	 for (i in 1:dim(hs)[1]){
		 #If Halfsib, then put the greater number first
		 if(hs[i,5]>hs[i,6]){points(hs[i,5],hs[i,6],pch=16,col="#33CCFF98")}else{points(hs[i,6],hs[i,5],pch=16,col="#33CCFF98")}
		 }
	 
	 fs<-subset(sibs,type=="Full"&Probability>prob)
	 for (i in 1:dim(fs)[1]){
		 #If Halfsib, then put the greater number first
		 if(fs[i,5]<fs[i,6]){points(fs[i,5],fs[i,6],pch=16,col="#FF669998")}else{points(fs[i,6],fs[i,5],pch=16,col="#FF669998")}
		 }
	 
	 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
