\name{run.colony}
\alias{run.colony}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
run.colony(colonyexec = "Colony2.exe", colonypath = "/Users/ZSL/Documents/IoZ/Colony/", datadir = "/Users/ZSL/Desktop/Test/", filename = "Test1.DAT", wait = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colonyexec}{ ~~Describe \code{colonyexec} here~~ }
  \item{colonypath}{ ~~Describe \code{colonypath} here~~ }
  \item{datadir}{ ~~Describe \code{datadir} here~~ }
  \item{filename}{ ~~Describe \code{filename} here~~ }
  \item{wait}{ ~~Describe \code{wait} here~~ }
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
function(colonyexec="Colony2.exe",colonypath="/Users/ZSL/Documents/IoZ/Colony/",datadir="/Users/ZSL/Desktop/Test/",filename="Test1.DAT",wait=TRUE){
	 #don't forget the trailing slash!
	 
	 cat("Be aware: this may take several minutes, hours, or even weeks to run, depending on the settings.")
	 
	 
	 current.wd<-getwd()
	 
	 #change this so that it copies the colony program to the directory, not the other way round.
	 
	 #Extract the output file name defined in the colony file.
	 readLines(paste(datadir,filename,sep=""),n=2)->x
	 outputfilename<-substring (x[2], 1, 20)
	 outputfilename <- sub("^[\t\n\f\r ]*", "",outputfilename) #remove leading whitespace
	 outputfilename <- sub("[\t\n\f\r ]*$", "", outputfilename); #remove trailing whitespace
	 outputfilename
	 
	 setwd(datadir)
	 
	 platform<-.Platform
	 
	 if(platform$OS.type=="unix"){
	 #Unix/MacOSX commands
	 
	system(paste("cp",paste(colonypath,colonyexec,sep=""),datadir,sep=" "))
	system(paste("mv",paste(datadir,filename,sep=""),paste(datadir,"Colony2.DAT",sep=""),sep=" "))
	system("./Colony2.exe",wait=wait)
	system(paste("mv",paste(datadir,"Colony2.DAT",sep=""),paste(datadir,filename,sep=""),sep=" "))
	system("rm Colony2.exe")




	 }else{if(platform$OS.type=="windows"){
	#Windows commands
	shell(paste("copy",paste(colonypath,colonyexec,sep=""),datadir,sep=" "))#Copy the colony exe file to the project directory	
	shell(paste("rename",paste(datadir,filename,sep=""),paste(datadir,"Colony2.DAT",sep=""),sep=" "))#Rename the colony dat file as Colony2.DAT	 
	shell.exec("Colony2.exe") #run colony2
	shell(paste("rename",paste(datadir,"Colony2.DAT",sep=""),paste(datadir,filename,sep=""),sep=" "))#Rename the colony dat file to original file name.	 
	shell("del Colony2.exe") #tidy up, remove colony2.exe	  
	
	}else{stop(paste("This function is not correctly configured to run on",platform$OS.type,"systems."))
	}}	 
	
	#reset wd
 setwd(current.wd)

	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
