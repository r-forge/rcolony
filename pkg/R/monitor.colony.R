
monitor.colony<-function(datadir,interv.t=3,last.few=10){

 # if(length(list.files(datadir,pattern=".Ne"))>0){stop("There are already files present in output directory. Remove them and start again.")}


MidResult<-NULL

  #Get start time
  t1<-Sys.time()
  
  elapsed.t<-0
  
  #while(length(list.files(datadir,pattern=".Ne"))<1){#run forever
  while(TRUE){#run forever
  
  t2<-Sys.time()
  elapsed.t<-as.numeric(t2-t1)
  
  if(elapsed.t>interv.t){
  
  fileok<-file.exists("temp.txt")
  
if(fileok==TRUE&!is.na(get.interm.data(datadir=datadir)[1,1])){
	MidResult2<-get.interm.data(datadir=datadir)
	MidResult<-rbind(MidResult,MidResult2)
	MidResult<-unique(MidResult)

x<-MidResult[,1]
y<-MidResult[,2]
  
plot(x,y,type="b",xlab="",ylab="")
title(xlab="NumIterate",ylab="CrLogL")

#add routine to delete contents if file is too big.
#50lines?

tmp<-readLines(paste(datadir,"temp.txt",sep=""))
n<-50
from<-if(length(tmp)-n>0){length(tmp)-n>0}else{1}
to<-length(tmp)

write.table(tmp[from:to],file="temp.txt",row.names=FALSE,col.names=FALSE,quote=FALSE)
print(MidResult)
cat("\n")

	}else{cat("Waiting...\n")}
  
  
 
  t1<-Sys.time()
  
  
  
  }
  }}



