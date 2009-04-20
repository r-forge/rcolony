get.interm.data<-function(v1="CrLogL",datadir){

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

