#Dynamic plotting experiments
monitor.colony<-function(datadir,interv.t=3,last.few=10){
  if(length(list.files(datadir,pattern=".Ne"))>0){stop("There are already files present in output directory. Remove them and start again.")}
  #Get start time
  t1<-Sys.time()
  
  elapsed.t<-0
  while(length(list.files(datadir,pattern=".Ne"))<1){#run forever
  t2<-Sys.time()
  elapsed.t<-as.numeric(t2-t1)
  
  if(elapsed.t>interv.t){
  
  fileok<-length(list.files(datadir,pattern=".MidResult"))>0
  col.names<-c("Date","Time","Run","Tmr","NumIterate","CrLogL","F1","F2","F3","FS","HSPair","FSPair","AssgnC1","AssgnC2","AssgnP1","AssgnP2")
  
  
  if(fileok==TRUE){
  
  paste(datadir,list.files(pattern=".MidResult")[1],sep="")
  MidResult<-read.table(paste(datadir,list.files(datadir,pattern=".MidResult")[1],sep=""),header=FALSE,col.names=col.names,skip=1)
  
  if(length(MidResult$NumIterate)<1){MidResult<-as.data.frame(matrix(rep(0,length(col.names)),ncol=length(col.names)))}
  
  
  }else{
  
  MidResult<-as.data.frame(matrix(rep(0,length(col.names)),ncol=length(col.names)))}
  
  names(MidResult)<-col.names
  x<-MidResult$NumIterate
  y<-MidResult$CrLogL
  
  plot(x,y,type="b",xlab="",ylab="")
  title(xlab="NumIterate",ylab="CrLogL")
  #legend("bottomright","Still Running...",text.col="red")
  
  t1<-Sys.time()
  }
  
  
  
  }
  }