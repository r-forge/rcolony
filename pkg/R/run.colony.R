`run.colony` <-
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

system("./Colony2.exe 2>&1 | tee temp.txt",wait=wait)

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

