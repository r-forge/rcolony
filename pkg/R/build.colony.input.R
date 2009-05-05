build.colony.input<-function(wd=getwd(),name="Colony2.DAT"){

colonyfile<-NULL

cat("This function will construct a Colony input file.\nPLEASE REFER TO THE HELP FILE ?build.colony.input.\n\n")
cat(paste("It will be called",name,"and be placed in",wd,"...\n\n\n"))

#Functions used here
is.whole <- function(a) { floor(a)==a }


#######################################################
#  ! C, Dataset name, Length<51
#######################################################

while(length(colonyfile$datasetname)==0){
cat("Enter dataset name (must be <51 characters).\n\n\n")
colonyfile$datasetname<-scan(n=1,what="character")
write(paste(colonyfile$datasetname,"! C, Dataset name, Length<51"),name,append=FALSE)}

#######################################################
#  ! C, Main output file name, Length<21
#######################################################
while(length(colonyfile$outfile)==0){
cat("Enter main output file name (must be <21 characters).\n\n\n")
colonyfile$outfile<-scan(n=1,what="character")
write(paste(colonyfile$outfile,"! C, Main output file name, Length<21"),name,append=TRUE)}

#######################################################
#  ! C, Dataset name, Length<51
#######################################################

while(length(colonyfile$note)==0){
cat("Enter one sentence to describe your dataset (no carriage returns!).\n\n\n")
colonyfile$note<-scan(n=1,what="character")}

#######################################################
#  ! I, Number of offspring in the sample
#######################################################
while(length(colonyfile$n.offspring)==0){
cat("Enter number of offspring in the sample.\n\n\n")
colonyfile$n.offspring<-as.numeric(scan(n=1,what="integer"))
write(paste(colonyfile$n.offspring,"! I, Number of offspring in the sample"),name,append=TRUE)

if(length(colonyfile$n.offspring)!=0){
#Whole number warning 
if(is.whole(colonyfile$n.offspring)==FALSE){
flush.console()
colonyfile<-colonyfile[which(names(colonyfile)!="n.offspring")]
;warning("The number of offspring must be a whole number!\n",immediate.=TRUE)}}

}


#######################################################
#  ! I, Number of loci
#######################################################
while(length(colonyfile$n.loci)==0){
cat("Enter number of loci.\n\n\n")
colonyfile$n.loci<-as.numeric(scan(n=1,what="integer"))
write(paste(colonyfile$n.loci,"! I, Number of loci"),name,append=TRUE)

if(length(colonyfile$n.loci)!=0){
#Whole number warning 
if(is.whole(colonyfile$n.loci)==FALSE){
flush.console()
colonyfile<-colonyfile[which(names(colonyfile)!="n.loci")]
;warning("The number of loci must be a whole number!\n",immediate.=TRUE)}}


}

#######################################################
#  ! I, Seed for random number generator
#######################################################
while(length(colonyfile$rseed)==0){
cat("Enter seed for random number generator.\n\n\n")
colonyfile$rseed<-as.numeric(scan(n=1,what="integer"))
write(paste(colonyfile$rseed,"! I, Seed for random number generator"),name,append=TRUE)}

#######################################################
#  ! B, 0/1=Not updating/updating allele frequency
#######################################################
cat("Should allele frequency be updated?\n\n\n")
switch(menu(c("Not updating allele frequency", "Updating allele frequency")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$updateallelefreq<-0, colonyfile$updateallelefreq<-1)
write(paste(colonyfile$updateallelefreq,"! B, 0/1=Not updating/updating allele frequency"),name,append=TRUE)

#######################################################
#  ! B, 0/1=Diploid species/HaploDiploid species
#######################################################
cat("What kind of species is it?\nSee help for definitions.\n\n")
switch(menu(c("Diploid species", "HaploDiploid species")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$ploidy<-0, colonyfile$ploidy<-1)
write(paste(colonyfile$ploidy,"! B, 0/1=Diploid species/HaploDiploid species"),name,append=TRUE)

#######################################################
#  ! B, 0/1=Polygamy/Monogamy for males & females
#######################################################
cat("Are males monogamous or polygamous?\nSee help for definitions.\n\n")
switch(menu(c("Males monogamous", "Males polygamous")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$malepolygamy<-0, colonyfile$malepolygamy<-1)

cat("Are females monogamous or polygamous?\nSee help for definitions.\n\n")
switch(menu(c("Females monogamous", "Females polygamous")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$femalepolygamy<-0, colonyfile$femalepolygamy<-1)
write(paste(colonyfile$malepolygamy,colonyfile$femalepolygamy,"! B, 0/1=Polygamy/Monogamy for males & females"),name,append=TRUE)

#######################################################
#  ! B,R,R : Use sibship prior, Y/N=1/0. If Yes, give mean paternal, maternal sibship size
#######################################################
cat("Use sibship prior?\n\n\n")
switch(menu(c("Yes", "No")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$sibship.prior<-1, colonyfile$sibship.prior<-0)

if(colonyfile$sibship.prior==0){
	colonyfile$sibship.prior.paternal<-0
	colonyfile$sibship.prior.maternal<-0
write(paste(colonyfile$sibship.prior,colonyfile$sibship.prior.paternal,colonyfile$sibship.prior.maternal,"! B,R,R : Use sibship prior, Y/N=1/0. If Yes, give mean paternal, maternal sibship size"),name,append=TRUE)
	}else{

while(length(colonyfile$sibship.prior.paternal)==0){
cat("Enter the paternal sibship size (number of sibships).\n\n\n")
colonyfile$sibship.prior.paternal<-as.numeric(scan(n=1,what="integer"))}

while(length(colonyfile$sibship.prior.maternal)==0){
cat("Enter the maternal sibship size (number of sibships).\n\n\n")
colonyfile$sibship.prior.maternal<-as.numeric(scan(n=1,what="integer"))}
write(paste(colonyfile$sibship.prior,colonyfile$sibship.prior.paternal,colonyfile$sibship.prior.maternal,"! B,R,R : Use sibship prior, Y/N=1/0. If Yes, give mean paternal, maternal sibship size"),name,append=TRUE)
}

#######################################################
#  ! B, 0/1=Unknown/Known population allele frequency
#######################################################
cat("Unknown/Known population allele frequency?\n\n\n")
switch(menu(c("Unknown", "Known")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$knownAFreq<-0, colonyfile$knownAFreq<-1)

write(paste(colonyfile$knownAFreq,"! B, 0/1=Unknown/Known population allele frequency"),name,append=TRUE)

if(colonyfile$knownAFreq==1){
while(length(colonyfile$AlleleFreqPATH)==0){
cat("Select the ALLELE FREQUENCY file.\n\n\n");Sys.sleep(.2)
flush.console()
colonyfile$AlleleFreqPATH<-file.choose()

#cat("What is the delimiter for this file?\n\n\n")
#flush.console()
#switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,
#       cat("Nothing done\n\n\n"), colonyfile$delim.for.allele.freq<-"", colonyfile$delim.for.allele.freq<-"\t", colonyfile$delim.for.allele.freq<-",",delim.for.allele.freq<-"Other")
#
#while(length(colonyfile$delim.for.allele.freq)=="Other"){
#if(colonyfile$delim.for.allele.freq=="Other"){
#cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
#colonyfile$delim.for.allele.freq<-scan(n=1,what="character")}}
colonyfile$delim.for.allele.freq<-""

colonyfile$allele.frequency<-read.table(colonyfile$AlleleFreqPATH,header=FALSE,colClasses=c("character"),sep=colonyfile$delim.for.allele.freq,fill=TRUE,flush=TRUE,na.string="") 

flush.console()

if(colonyfile$n.loci!=dim(colonyfile$allele.frequency)[1]/2){colonyfile<-colonyfile[which(names(colonyfile)!="AlleleFreqPATH")]
;warning(paste("The number of defined loci ","(", colonyfile$n.loci,") does not equal the number of markers provided in the file selected (", dim(colonyfile$allele.frequency)[1]/2,").\n\n",sep=""),immediate.=TRUE)}
}

colonyfile$allele.frequency[,1+dim(colonyfile$allele.frequency)[2]]<-c("!Allele frequency",rep("",dim(colonyfile$allele.frequency)[1]-1))
write.table(colonyfile$allele.frequency,name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE,na="")}





#######################################################
#  ! I, Number of runs
#######################################################
while(length(colonyfile$n.runs)==0){
cat("Number of runs.\n\n\n")
colonyfile$n.runs<-as.numeric(scan(n=1,what="integer"))
write(paste(colonyfile$n.runs,"! I, Number of runs"),name,append=TRUE)

if(length(colonyfile$n.runs)!=0){
#Whole number warning 
if(is.whole(colonyfile$n.runs)==FALSE){
flush.console()
colonyfile<-colonyfile[which(names(colonyfile)!="n.runs")]
;warning("The number of runs must be a whole number!\n",immediate.=TRUE)}}

}

#######################################################
#  ! I, Length of Run (1, 2, 3) = (Short, Medium, Long)
#######################################################
while(length(colonyfile$runlength)==0){
cat("Length of run?\n\n\n")
switch(menu(c("Short", "Medium","Long")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$runlength<-1, colonyfile$runlength<-2, colonyfile$runlength<-3)
write(paste(colonyfile$runlength,"! I, Length of Run (1, 2, 3) = (Short, Medium, Long)"),name,append=TRUE)}

#######################################################
#  ! B, 0/1=Monitor method by Iterate#/Time in second
#######################################################
cat("Monitor method by Iterate/Time in second?\n\n\n")
switch(menu(c("Monitor by iterate", "Monitor by time in seconds")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$monitortype<-0, colonyfile$monitortype<-1)
write(paste(colonyfile$monitortype,"! B, 0/1=Monitor method by Iterate#/Time in second"),name,append=TRUE)

#######################################################
#  ! I, Monitor interval in Iterate#/Seconds
#######################################################
while(length(colonyfile$interval)==0){
cat("Monitor interval (in iterate number or seconds) depending on how you have chosen to monitor progress.\n\n\n")
colonyfile$interval<-as.numeric(scan(n=1,what="integer"))
write(paste(colonyfile$interval,"! I, Monitor interval in Iterate#/Seconds"),name,append=TRUE)}

#######################################################
#  ! B, 0/1=Other platform/Windows execution
#######################################################
cat("What platform is this to be executed on?\n\n\n")
switch(menu(c("Microsoft Windows system", "Other system (e.g. Mac/Unix)")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$sys<-1, colonyfile$sys<-0)
write(paste(colonyfile$sys,"! B, 0/1=Other platform/Windows execution"),name,append=TRUE)

#######################################################
#  ! 1/0=Full-likelihood/pair-likelihood score method
#######################################################
cat("Which likelihood method should be used?\n\n\n")
switch(menu(c("Full likelihood", "Pairwise likelihood")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$likelihood.method<-1, colonyfile$likelihood.method<-0)
write(paste(colonyfile$likelihood.method,"! 1/0=Full-likelihood/pair-likelihood score method"),name,append=TRUE)

#######################################################
#  ! 1/2/3=low/medium/high precision
#######################################################
cat("What level of precision should be used?\n\n\n")
switch(menu(c("Low", "Medium","High")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$precision<-1, colonyfile$precision<-2, colonyfile$precision<-3)
write(paste(colonyfile$precision,"! 1/2/3=low/medium/high precision"),name,append=TRUE)

write("\n",name,append=TRUE)

#######################################################
#Marker file import
#######################################################

#Give the path to the marker types and error rate file. This should be a file with a number of columns equal to the number of markers used.
#There should be 4 rows, 1) marker ID, 2) marker type, 3) marker specific allelic dropout rate, 4) marker specific other typing error rate.

while(length(colonyfile$MarkerPATH)==0){
cat("Provide the path to the Marker Types and Error Rate file.\n\n\n");Sys.sleep(.5)
flush.console()
colonyfile$MarkerPATH<-file.choose()

#cat("What is the delimiter for this file?\n\n\n")
#flush.console()
#switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,
#       cat("Nothing done\n\n\n"), colonyfile$delim.for.markers<-"", colonyfile$delim.for.markers<-"\t", colonyfile$delim.for.markers<-",",delim.for.markers<-"Other")
#
#while(length(colonyfile$delim.for.markers)=="Other"){
#if(colonyfile$delim.for.markers=="Other"){
#cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
#colonyfile$delim.for.markers<-scan(n=1,what="character")}}
colonyfile$delim.for.markers<-""

colonyfile$Markers<-read.table(colonyfile$MarkerPATH,header=FALSE,colClasses=c("character"),sep=colonyfile$delim.for.markers) 

flush.console()

if(colonyfile$n.loci!=dim(colonyfile$Markers)[2]){colonyfile<-colonyfile[which(names(colonyfile)!="MarkerPATH")]
;warning(paste("The number of defined loci ","(", colonyfile$n.loci,") does not equal the number of markers provided in the file selected (", dim(colonyfile$Markers)[2],").\n\n",sep=""),immediate.=TRUE)}
}

colonyfile$Markers[,1+dim(colonyfile$Markers)[2]]<-c("!Marker IDs","!Marker types, 0/1=Codominant/Dominant","!Marker-specific allelic dropout rate","!Other marker-specific typing-error rate")
write.table(colonyfile$Markers,name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)


#######################################################
#Offspring genotype import
#######################################################

#Give the path to the offpring ID and genotype file
#This should have a first column giving the ID, then 2 columns for each locus (1 for each allele at that locus), at least for diploid species.
#Therefore, with 4 loci, there should be 9 columns.

cat("\nProvide the path to the offspringID and genotype file.\n\n\n")
flush.console()
while(length(colonyfile$OSGenotypePATH)==0){
colonyfile$OSGenotypePATH<-file.choose()


#cat("What is the delimiter for this file?\n\n\n")
#flush.console()
#switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,
#       cat("Nothing done\n\n\n"), colonyfile$delim.for.OSGenotype<-"", colonyfile$delim.for.OSGenotype<-"\t", colonyfile$delim.for.OSGenotype<-",",delim.for.OSGenotype<-"Other")
#
#while(length(colonyfile$delim.for.OSGenotype)=="Other"){
#if(colonyfile$delim.for.OSGenotype=="Other"){
#cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
#colonyfile$delim.for.OSGenotype<-scan(n=1,what="character")}}
colonyfile$delim.for.OSGenotype<-""


colonyfile$Offspring<-read.table(colonyfile$OSGenotypePATH,header=FALSE,colClasses=c("character"),sep=colonyfile$delim.for.OSGenotype) 
if(colonyfile$n.offspring!=dim(colonyfile$Offspring)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="OSGenotypePATH")];
flush.console();
warning(paste("The number of defined offspring ","(", colonyfile$n.offspring,") does not equal the number of offspring provided in the file selected (", dim(colonyfile$Offspring)[1],").\n\n",sep=""),immediate.=TRUE)}

fileloci<-(dim(colonyfile$Offspring)[2]-1)/2
if(colonyfile$ploidy==0){if((colonyfile$n.loci)!=fileloci){
colonyfile<-colonyfile[which(names(colonyfile)!="OSGenotypePATH")];
flush.console();
warning(paste("The number of defined loci ","(", colonyfile$n.loci,") does not appear to equal the number of loci provided in the file selected (", fileloci,").\n\n",sep=""),immediate.=TRUE)}}
}

colonyfile$Offspring[,1+dim(colonyfile$Offspring)[2]]<-c("!Offspring ID and genotypes",rep("",dim(colonyfile$Offspring)[1]-1))
write.table(colonyfile$Offspring,name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)

######################################################
#Sampling of candidate parents
######################################################


#######################################################
#FATHERS - probability of inclusion in candidate set
#######################################################
while(length(colonyfile$fatherprob)==0){
cat("What is the probability that the FATHER of an offpring is included in the candidate set?\n\n\n E.g. 0.5\n\n\n")
colonyfile$fatherprob<-as.numeric(scan(n=1,what="integer"))
if(colonyfile$fatherprob>1){
flush.console()
cat("Probabilities must be less than or equal to 1.\n")
colonyfile<-colonyfile[which(names(colonyfile)!="fatherprob")]}
}

#######################################################
#Number of candidate fathers
#######################################################
while(length(colonyfile$n.father)==0){
cat("How many candidate FATHERS are there?\n\n\n")
colonyfile$n.father<-as.numeric(scan(n=1,what="integer"))

if(length(colonyfile$n.father)!=0){
#Whole number warning 
if(is.whole(colonyfile$n.father)==FALSE){
flush.console()
colonyfile<-colonyfile[which(names(colonyfile)!="n.fathers")]
;warning("The number of fathers must be a whole number!\n",immediate.=TRUE)}}
}

#######################################################
#Import candidate FATHERS file
#######################################################
while(length(colonyfile$fathersPATH)==0){
cat("Provide the path to the candidate FATHERS file.\n\n\n")
flush.console()
colonyfile$fathersPATH<-file.choose()

#cat("What is the delimiter for this file?\n\n\n")
#flush.console()
#switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,
#       cat("Nothing done\n\n\n"), colonyfile$delim.for.fathers<-"", colonyfile$delim.for.fathers<-"\t", colonyfile$delim.for.fathers<-",",delim.for.fathers<-"Other")
#
#while(length(colonyfile$delim.for.fathers)=="Other"){
#if(colonyfile$delim.for.fathers=="Other"){
#cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
#colonyfile$delim.for.fathers<-scan(n=1,what="character")}}
colonyfile$delim.for.fathers<-""

colonyfile$fathers<-read.table(colonyfile$fathersPATH,header=FALSE,sep=colonyfile$delim.for.fathers,colClasses=c("character"))
if(colonyfile$n.father!=dim(colonyfile$fathers)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="fathersPATH")];
flush.console();
warning(paste("The number of defined FATHERS ","(", colonyfile$n.father,") does not equal the number of FATHERS provided in the file selected (", dim(colonyfile$fathers)[1],").\n\n",sep=""),immediate.=TRUE)}
}

#######################################################
#MOTHERS - probability of inclusion in candidate set
#######################################################
while(length(colonyfile$motherprob)==0){
cat("What is the probability that the MOTHER of an offpring is included in the candidate set?\n\n\n E.g. 0.5\n\n\n")
colonyfile$motherprob<-as.numeric(scan(n=1,what="integer"))
if(colonyfile$motherprob>1){
flush.console()
cat("Probabilities must be less than or equal to 1.\n")
colonyfile<-colonyfile[which(names(colonyfile)!="motherprob")]}
}

#######################################################
#Number of candidate mothers
#######################################################
while(length(colonyfile$n.mother)==0){
cat("How many candidate MOTHERS are there?\n\n\n")
colonyfile$n.mother<-as.numeric(scan(n=1,what="integer"))


if(length(colonyfile$n.mother)!=0){
#Whole number warning 
if(is.whole(colonyfile$n.mother)==FALSE){
flush.console()
colonyfile<-colonyfile[which(names(colonyfile)!="n.mothers")]
;warning("The number of mothers must be a whole number!\n",immediate.=TRUE)}}
}

#######################################################
#Import candidate MOTHERS
#######################################################
while(length(colonyfile$mothersPATH)==0){
cat("Provide the path to the candidate MOTHERS file.\n\n\n")
flush.console()
colonyfile$mothersPATH<-file.choose()

flush.console()
#cat("What is the delimiter for this file?\n\n\n")
#switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,
#       cat("Nothing done\n\n\n"), colonyfile$delim.for.mothers<-"", colonyfile$delim.for.mothers<-"\t", colonyfile$delim.for.mothers<-",",delim.for.mothers<-"Other")
#
#while(length(colonyfile$delim.for.mothers)=="Other"){
#if(colonyfile$delim.for.mothers=="Other"){
#cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
#colonyfile$delim.for.mothers<-scan(n=1,what="character")}}
colonyfile$delim.for.mothers<-""

colonyfile$mothers<-read.table(colonyfile$mothersPATH,header=FALSE,sep=colonyfile$delim.for.mothers,colClasses=c("character")) 
if(colonyfile$n.mother!=dim(colonyfile$mothers)[1]){colonyfile<-colonyfile[which(names(colonyfile)!="mothersPATH")];
flush.console();
warning(paste("The number of defined MOTHERS ","(", colonyfile$n.mother,") does not equal the number of MOTHERS provided in the file selected (", dim(colonyfile$mothers)[1],").\n\n",sep=""),immediate.=TRUE)}
}

write(paste(colonyfile$fatherprob,colonyfile$motherprob,"!Probabilities that the father and mother of an offspring included in candidates"),name,append=TRUE)
write(paste(colonyfile$n.father,colonyfile$n.mother,"!Numbers of candidate males and females"),name,append=TRUE)
write("",name,append=TRUE)

colonyfile$fathers[,1+dim(colonyfile$fathers)[2]]<-c("!Candidate M ID and genotypes",rep("",dim(colonyfile$fathers)[1]-1))
write.table(colonyfile$fathers,name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)

colonyfile$mothers[,1+dim(colonyfile$mothers)[2]]<-c("!Candidate F ID and genotypes",rep("",dim(colonyfile$mothers)[1]-1))
write.table(colonyfile$mothers,name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)


#######################################################
#Define known PATERNAL diads
#######################################################

while(length(colonyfile$n.known.paternal.diads)==0){
cat("Enter the number of known offspring-PATERNAL diads.\n\n\n")
colonyfile$n.known.paternal.diads<-as.numeric(scan(n=1,what="integer"))

if(length(colonyfile$n.known.paternal.diads)!=0){
#Whole number warning 
if(is.whole(colonyfile$n.known.paternal.diads)==FALSE){
flush.console()
colonyfile<-colonyfile[which(names(colonyfile)!="n.known.paternal.diads")]
;warning("The number of known paternal diads must be a whole number!\n",immediate.=TRUE)}}
}

if(colonyfile$n.known.paternal.diads>0){

#If there are some known paternal diads...

#Get the path, and delimiter, to the file...
while(length(colonyfile$paternal.diads.PATH)==0){
	cat("Provide the path to the PATERNAL diads file.\n\n\n")
	flush.console()
	colonyfile$paternal.diads.PATH<-file.choose()

#	cat("What is the delimiter for this file?\n\n\n")
#	flush.console()
#	switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,cat("Nothing done\n\n\n"), colonyfile$delim.for.paternal.sibs.PATH<-"", colonyfile$delim.for.paternal.sibs.PATH<-"\t", colonyfile$delim.for.paternal.sibs.PATH<-",",delim.for.paternal.sibs.PATH<-"Other")
#
#		#Caveat for if the delimiter is OTHER
#		while(length(colonyfile$delim.for.paternal.diads.PATH)=="Other"){
#		if(colonyfile$delim.for.paternal.diads.PATH=="Other"){
#		cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
#		colonyfile$delim.for.paternal.diads.PATH<-scan(n=1,what="character")}}
colonyfile$delim.for.paternal.diads.PATH<-""

#Read in the data...
colonyfile$known.paternal.diads<-read.table(colonyfile$paternal.diads.PATH,header=FALSE,sep=colonyfile$delim.for.paternal.diads.PATH,colClasses=c("character"))

#Check the data
if(colonyfile$n.known.paternal.diads!=dim(colonyfile$known.paternal.diads)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="paternal.diads.PATH")];
flush.console();
warning(paste("The number of defined paternal diads ","(", colonyfile$n.paternal.sibs.or.paternities,") does not equal the number of paternal diads provided in the file selected (", dim(colonyfile$known.paternal.diads)[1],").\n\n",sep=""),immediate.=TRUE)
}

#Check the data
#if(colonyfile$n.known.paternal.diads!=dim(colonyfile$known.paternal.diads)[1]){
#colonyfile<-colonyfile[which(names(colonyfile)!="paternal.diads.PATH")];
#flush.console();
#warning(paste("The number of defined paternal diads ","(", colonyfile$n.paternal.sibs.or.paternities,") does not equal the number of paternal diads provided in the file selected (", dim(colonyfile$known.paternal.diads)[1],").\n\n",sep=""),immediate.=TRUE)
#}

#if this is true, then all offspring in the diad file are present in the offspring genotype file
if(sum(colonyfile$known.paternal.diads$V1%in%colonyfile$Offspring[,1])==length(colonyfile$known.paternal.diads$V1)){}else{
colonyfile<-colonyfile[which(names(colonyfile)!="paternal.diads.PATH")];
flush.console();
warning(paste("Offspring in diad file are not present in the offspring genotype data:",paste(colonyfile$known.paternal.diads$V1[which(colonyfile$known.paternal.diads$V1%in%colonyfile$Offspring[,1]==FALSE)], collapse=", ")))
}

if(sum(colonyfile$known.paternal.diads$V2%in%colonyfile$fathers[,1])==length(colonyfile$known.paternal.diads$V2)){}else{
colonyfile<-colonyfile[which(names(colonyfile)!="paternal.diads.PATH")];
flush.console();
warning(paste("Fathers in diad file are not present in the father genotype data:",paste(colonyfile$known.paternal.diads$V2[which(colonyfile$known.paternal.diads$V2%in%colonyfile$Offspring[,1]==FALSE)], collapse=", ")))
}


}


write.table(paste(colonyfile$n.known.paternal.diads,"!Number of known paternities"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
colonyfile$known.paternal.diads[,1+dim(colonyfile$known.paternal.diads)[2]]<-c("!IDs of known offspring-father dyad",rep("",dim(colonyfile$known.paternal.diads)[1]-1))

write.table(colonyfile$known.paternal.diads,name,append=TRUE,quote=FALSE,na=" ",row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)

}else{
#If there are no known paternal diads
write.table(paste(colonyfile$n.known.paternal.diads,"!Number of known paternities"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)
}

#######################################################
#Define MATERNAL diads
#######################################################

while(length(colonyfile$n.known.maternal.diads)==0){
cat("Enter the number of known offspring-MATERNAL diads.\n\n\n")
colonyfile$n.known.maternal.diads<-as.numeric(scan(n=1,what="integer"))

if(length(colonyfile$n.known.maternal.diads)!=0){
#Whole number warning 
if(is.whole(colonyfile$n.known.maternal.diads)==FALSE){
flush.console()
colonyfile<-colonyfile[which(names(colonyfile)!="n.known.maternal.diads")]
;warning("The number of known maternal diads must be a whole number!\n",immediate.=TRUE)}}
}

if(colonyfile$n.known.maternal.diads>0){

#If there are some known maternal diads...

#Get the path, and delimiter, to the file...
while(length(colonyfile$maternal.diads.PATH)==0){
	cat("Provide the path to the MATERNAL diads file.\n\n\n")
	flush.console()
	colonyfile$maternal.diads.PATH<-file.choose()

	#cat("What is the delimiter for this file?\n\n\n")
#	flush.console()
#	switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,cat("Nothing done\n\n\n"), colonyfile$delim.for.maternal.sibs.PATH<-"", colonyfile$delim.for.maternal.sibs.PATH<-"\t", colonyfile$delim.for.maternal.sibs.PATH<-",",delim.for.maternal.sibs.PATH<-"Other")
#
#		#Caveat for if the delimiter is OTHER
#		while(length(colonyfile$delim.for.maternal.diads.PATH)=="Other"){
#		if(colonyfile$delim.for.maternal.diads.PATH=="Other"){
#		cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
#		colonyfile$delim.for.maternal.diads.PATH<-scan(n=1,what="character")}}
colonyfile$delim.for.maternal.diads.PATH<-""

#Read in the data...
colonyfile$known.maternal.diads<-read.table(colonyfile$maternal.diads.PATH,header=FALSE,sep=colonyfile$delim.for.maternal.diads.PATH,colClasses=c("character"))

#Check the data
if(colonyfile$n.known.maternal.diads!=dim(colonyfile$known.maternal.diads)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="maternal.diads.PATH")];
flush.console();
warning(paste("The number of defined maternal diads ","(", colonyfile$n.known.maternal.diads,") does not equal the number of maternal diads provided in the file selected (", dim(colonyfile$known.maternal.diads)[1],").\n\n",sep=""),immediate.=TRUE)
}

#if this is true, then all offspring in the diad file are present in the offspring genotype file
if(sum(colonyfile$known.maternal.diads$V1%in%colonyfile$Offspring[,1])==length(colonyfile$known.maternal.diads$V1)){}else{
colonyfile<-colonyfile[which(names(colonyfile)!="maternal.diads.PATH")];
flush.console();
warning(paste("Offspring in diad file are not present in the offspring genotype data:",paste(colonyfile$known.maternal.diads$V1[which(colonyfile$known.maternal.diads$V1%in%colonyfile$Offspring[,1]==FALSE)], collapse=", ")))
}

if(sum(colonyfile$known.maternal.diads$V2%in%colonyfile$Mothers[,1])==length(colonyfile$known.maternal.diads$V2)){}else{
colonyfile<-colonyfile[which(names(colonyfile)!="maternal.diads.PATH")];
flush.console();
warning(paste("Mothers in diad file are not present in the mother genotype data:",paste(colonyfile$known.maternal.diads$V2[which(colonyfile$known.maternal.diads$V2%in%colonyfile$Offspring[,1]==FALSE)], collapse=", ")))
}

}


write.table(paste(colonyfile$n.known.maternal.diads," !Number of known maternities"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
colonyfile$known.maternal.diads[,1+dim(colonyfile$known.maternal.diads)[2]]<-c("!IDs of known offspring-mother dyad",rep("",dim(colonyfile$known.maternal.diads)[1]-1))

write.table(colonyfile$known.maternal.diads,name,append=TRUE,quote=FALSE,na=" ",row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)

}else{
#If there are no known maternal diads
write.table(paste(colonyfile$n.known.maternal.diads," !Number of known offspring-mother dyad"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)
}
 
#######################################################
#Define PATERNAL sibships
#######################################################


while(length(colonyfile$n.paternal.sibships)==0){
cat("Enter the number of known PATERNAL sibship/paternity.\nA known paternal sibship contains all of the offspring in the offspring sample who are known to share the same father no matter whether the father is known or not (unknown fathers are coded with 0).\n\n")
colonyfile$n.paternal.sibships<-as.numeric(scan(n=1,what="integer"))

if(length(colonyfile$n.paternal.sibships)!=0){
#Whole number warning 
if(is.whole(colonyfile$n.paternal.sibships)==FALSE){
flush.console()
colonyfile<-colonyfile[which(names(colonyfile)!="n.paternal.sibships")];
warning("The number of known paternal sibships must be a whole number!\n",immediate.=TRUE)}}
}


if(colonyfile$n.paternal.sibships>0){

#If there are some known sibships...

#Get the path, and delimiter, to the file...
while(length(colonyfile$paternal.sibs.PATH)==0){
	cat("Provide the path to the candidate PATERNAL SIBSHIPS file.\n\n\n")
	flush.console()
	colonyfile$paternal.sibs.PATH<-file.choose()

#	cat("What is the delimiter for this file?\n\n\n")
#	flush.console()
#	switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,cat("Nothing done\n\n\n"), colonyfile$delim.for.paternal.sibs.PATH<-"", colonyfile$delim.for.paternal.sibs.PATH<-"\t", colonyfile$delim.for.paternal.sibs.PATH<-",",delim.for.paternal.sibs.PATH<-"Other")
#
#		#Caveat for if the delimiter is OTHER
#		while(length(colonyfile$delim.for.paternal.sibs.PATH)=="Other"){
#		if(colonyfile$delim.for.paternal.sibs.PATH=="Other"){
#		cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
#		colonyfile$delim.for.paternal.sibs.PATH<-scan(n=1,what="character")}}
colonyfile$delim.for.paternal.sibs.PATH<-""

#Read in the data...
colonyfile$paternal.sibships<-read.table(colonyfile$paternal.sibs.PATH,header=FALSE,sep=colonyfile$delim.for.paternal.sibs.PATH,colClasses=c("character"),fill=TRUE,flush=TRUE,na.strings="")

#Check the data
if(colonyfile$n.paternal.sibships!=dim(colonyfile$paternal.sibships)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="paternal.sibs.PATH")];
flush.console();
warning(paste("The number of defined paternal sibs/paternities ","(", colonyfile$n.paternal.sibships,") does not equal the number of paternities provided in the file selected (", dim(colonyfile$paternal.sibships)[1],").\n\n",sep=""),immediate.=TRUE)
}

#Futher checks
#if this is true, then all offspring in the diad file are present in the offspring genotype file
if(sum(colonyfile$paternal.sibships$V1%in%colonyfile$fathers[,1])==length(colonyfile$paternal.sibships$V1)){}else{
colonyfile<-colonyfile[which(names(colonyfile)!="paternal.sibs.PATH")];
flush.console();
warning(paste("Fathers in paternal sibships file are not present in the fathers genotype data:",paste(colonyfile$paternal.sibships$V1[which(colonyfile$paternal.sibships$V1%in%colonyfile$fathers[,1]==FALSE)], collapse=", ")))
}

os<-na.omit(as.vector(as.matrix(colonyfile$paternal.sibships[,2:dim(colonyfile$paternal.sibships)[2]])))

if(sum(os%in%colonyfile$fathers[,1])==length(os)){}else{
colonyfile<-colonyfile[which(names(colonyfile)!="paternal.sibs.PATH")];
flush.console();
warning(paste("Offspring in paternal sibships file are not present in the fathers genotype data:",paste(os[which(os%in%colonyfile$fathers[,1]==FALSE)], collapse=", ")))
}

}	


write.table(paste(colonyfile$n.paternal.sibships," !Number of known paternal sibships"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)

#Output KnownPaternity.txt
if(colonyfile$n.paternal.sibships>0){
temp1<-as.data.frame(colonyfile$paternal.sibships)
names(temp1)<-c("FatherID",paste("OffspringID",1:(dim(temp1)[2]-1),sep=""))
write.table(temp1,"KnownPaternity.txt",row.names=FALSE,quote=FALSE,col.names=TRUE)}

#remove first column
colonyfile$paternal.sibships<-colonyfile$paternal.sibships[,2:dim(colonyfile$paternal.sibships)[2]]
colonyfile$paternal.sibships[,1+dim(colonyfile$paternal.sibships)[2]]<-c("!Size of known paternal sibship, and IDs of offspring in the sibship",rep("",dim(colonyfile$paternal.sibships)[1]-1))

csum<-NULL
for (i in 1:dim(colonyfile$paternal.sibships)[1]){
csum[i]<-length(colonyfile$paternal.sibships[i,][!is.na(colonyfile$paternal.sibships[i,])])}
csum<-csum-1

colonyfile$paternal.sibships<-cbind(csum,colonyfile$paternal.sibships)

write.table(colonyfile$paternal.sibships,name,append=TRUE,quote=FALSE,na=" ",row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)

}else{
#If there are no known sibships
write.table(paste(colonyfile$n.paternal.sibships," !Number of known paternal sibships"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)
}

#######################################################
#Define MATERNAL sibships
#######################################################

while(length(colonyfile$n.maternal.sibships)==0){
cat("Enter the number of known MATERNAL sibship/paternity.\nA known maternal sibship contains all of the offspring in the offspring sample who are known to share the same mother no matter whether the mother is known or not (unknown mothers are coded with 0).\n\n")
colonyfile$n.maternal.sibships<-as.numeric(scan(n=1,what="integer"))

if(length(colonyfile$n.maternal.sibships)!=0){
#Whole number warning 
if(is.whole(colonyfile$n.maternal.sibships)==FALSE){
flush.console()
colonyfile<-colonyfile[which(names(colonyfile)!="n.maternal.sibships")];
warning("The number of known maternal sibships must be a whole number!\n",immediate.=TRUE)}}
}


if(colonyfile$n.maternal.sibships>0){

#If there are some known sibships...

#Get the path, and delimiter, to the file...
while(length(colonyfile$maternal.sibs.PATH)==0){
	cat("Provide the path to the MATERNAL SIBSHIPS file.\n\n\n")
	flush.console()
	colonyfile$maternal.sibs.PATH<-file.choose()

#	cat("What is the delimiter for this file?\n\n\n")
#	flush.console()
#	switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,cat("Nothing done\n\n\n"), colonyfile$delim.for.maternal.sibs.PATH<-"", colonyfile$delim.for.maternal.sibs.PATH<-"\t", colonyfile$delim.for.maternal.sibs.PATH<-",",delim.for.maternal.sibs.PATH<-"Other")
#
#		#Caveat for if the delimiter is OTHER
#		while(length(colonyfile$delim.for.maternal.sibs.PATH)=="Other"){
#		if(colonyfile$delim.for.maternal.sibs.PATH=="Other"){
#		cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
#		colonyfile$delim.for.maternal.sibs.PATH<-scan(n=1,what="character")}}
colonyfile$delim.for.maternal.sibs.PATH<-""

#Read in the data...
colonyfile$maternal.sibships<-read.table(colonyfile$maternal.sibs.PATH,header=FALSE,sep=colonyfile$delim.for.maternal.sibs.PATH,colClasses=c("character"),fill=TRUE,flush=TRUE,na.strings="")

#Check the data
if(colonyfile$n.maternal.sibships!=dim(colonyfile$maternal.sibships)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="maternal.sibs.PATH")];
flush.console();
warning(paste("The number of defined maternal sibs/maternities ","(", colonyfile$n.maternal.sibships,") does not equal the number of maternities provided in the file selected (", dim(colonyfile$maternal.sibships)[1],").\n\n",sep=""),immediate.=TRUE)
}

#Futher checks
#if this is true, then all offspring in the diad file are present in the offspring genotype file
if(sum(colonyfile$maternal.sibships$V1%in%colonyfile$mothers[,1])==length(colonyfile$maternal.sibships$V1) ){}else{
colonyfile<-colonyfile[which(names(colonyfile)!="maternal.sibs.PATH")];
flush.console();
warning(paste("Mothers in maternal sibships file are not present in the mothers genotype data:",paste(colonyfile$maternal.sibships$V1[which(colonyfile$maternal.sibships$V1%in%colonyfile$mothers[,1]==FALSE)], collapse=", ")))
}

os<-na.omit(as.vector(as.matrix(colonyfile$maternal.sibships[,2:dim(colonyfile$maternal.sibships)[2]])))

if(sum(os%in%colonyfile$fathers[,1])==length(os)){}else{
colonyfile<-colonyfile[which(names(colonyfile)!="maternal.sibs.PATH")];
flush.console();
warning(paste("Offspring in maternal sibships file are not present in the mothers genotype data:",paste(os[which(os%in%colonyfile$mothers[,1]==FALSE)], collapse=", ")))
}


}

write.table(paste(colonyfile$n.maternal.sibships," !Number of known maternal sibships"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)

#Output KnownMaternity.txt
if(colonyfile$n.maternal.sibships>0){
temp1<-as.data.frame(colonyfile$maternal.sibships)
names(temp1)<-c("MotherID",paste("OffspringID",1:(dim(temp1)[2]-1),sep=""))
write.table(temp1,"KnownMaternity.txt",row.names=FALSE,quote=FALSE,col.names=TRUE)}


#remove first column
colonyfile$maternal.sibships<-colonyfile$maternal.sibships[,2:dim(colonyfile$maternal.sibships)[2]]
colonyfile$maternal.sibships[,1+dim(colonyfile$maternal.sibships)[2]]<-c("!Size of known maternal sibship, and IDs of offspring in the sibship",rep("",dim(colonyfile$maternal.sibships)[1]-1))

csum<-NULL
for (i in 1:dim(colonyfile$maternal.sibships)[1]){
csum[i]<-length(colonyfile$maternal.sibships[i,][!is.na(colonyfile$maternal.sibships[i,])])}
csum<-csum-1

colonyfile$maternal.sibships<-cbind(csum,colonyfile$maternal.sibships)

write.table(colonyfile$maternal.sibships,name,append=TRUE,quote=FALSE,na=" ",row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)

}else{
#If there are no known sibships
write.table(paste(colonyfile$n.maternal.sibships," !Number of known maternal sibships"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)
}

#######################################################
#Define excluded PATERNITIES
#######################################################
 
while(length(colonyfile$n.excluded.paternities)==0){
cat("Enter the number of offspring with known excluded PATERNITY.\n\n\n")
colonyfile$n.excluded.paternities<-as.numeric(scan(n=1,what="integer"))

if(length(colonyfile$n.excluded.paternities)!=0){
#Whole number warning 
if(is.whole(colonyfile$n.excluded.paternities)==FALSE){
flush.console()
colonyfile<-colonyfile[which(names(colonyfile)!="n.excluded.paternities")];
warning("The number of excluded paternities must be a whole number!\n",immediate.=TRUE)}}
}

if(colonyfile$n.excluded.paternities>0){


#Get the path, and delimiter, to the file...
while(length(colonyfile$excluded.paternities.PATH)==0){
	cat("Provide the path to the excluded PATERNITY file.\n\n\n")
	flush.console()
	colonyfile$excluded.paternities.PATH<-file.choose()

#	cat("What is the delimiter for this file?\n\n\n")
#	flush.console()
#	switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,cat("Nothing done\n\n\n"), colonyfile$delim.for.excluded.paternities.PATH<-"", colonyfile$delim.for.excluded.paternities.PATH<-"\t", colonyfile$delim.for.excluded.paternities.PATH<-",",delim.for.excluded.paternities.PATH<-"Other")
#
#		#Caveat for if the delimiter is OTHER
#		while(length(colonyfile$delim.for.excluded.paternities.PATH)=="Other"){
#		if(colonyfile$delim.for.excluded.paternities.PATH=="Other"){
#		cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
#		colonyfile$delim.for.excluded.paternities.PATH<-scan(n=1,what="character")}}
colonyfile$delim.for.excluded.paternities.PATH<-""

#Read in the data...
colonyfile$excluded.paternities<-read.table(colonyfile$excluded.paternities.PATH,header=FALSE,sep=colonyfile$delim.for.excluded.paternities.PATH,colClasses=c("character"),fill=TRUE,flush=TRUE,na.strings="")


if(colonyfile$n.excluded.paternities>0){#Write ExcludedPaternity.txt file
temp1<-as.data.frame(colonyfile$excluded.paternities)
names(temp1)<-c("OffspringID",paste("ExcludedFatherID",1:(dim(temp1)[2]-1),sep=""))
write.table(temp1,"ExcludedPaternity.txt",row.names=FALSE,quote=FALSE,col.names=TRUE)}

#Check the data
if(colonyfile$n.excluded.paternities!=dim(colonyfile$excluded.paternities)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="excluded.paternities.PATH")];
flush.console();
warning(paste("The number of defined excluded paternities ","(", colonyfile$n.excluded.paternities,") does not equal the number provided in the file selected (", dim(colonyfile$excluded.paternities)[1],").\n\n",sep=""),immediate.=TRUE)
}


#Futher checks
#if this is true, then all offspring in the diad file are present in the offspring genotype file
if(sum(colonyfile$excluded.paternities$V1%in%colonyfile$Offspring[,1])==length(colonyfile$excluded.paternities$V1)){}else{
colonyfile<-colonyfile[which(names(colonyfile)!="excluded.paternities.PATH")];
flush.console();
warning(paste("Offspring in excluded paternities file are not present in the offspring genotype data:",paste(colonyfile$excluded.paternities$V1[which(colonyfile$excluded.paternities$V1%in%colonyfile$fathers[,1]==FALSE)], collapse=", ")))
}

os<-na.omit(as.vector(as.matrix(colonyfile$excluded.paternities[,2:dim(colonyfile$excluded.paternities)[2]])))

if(sum(os%in%colonyfile$fathers[,1])==length(os)){}else{
colonyfile<-colonyfile[which(names(colonyfile)!="excluded.paternities.PATH")];
flush.console();
warning(paste("Fathers in excluded paternities file are not present in the fathers genotype data:",paste(os[which(os%in%colonyfile$fathers[,1]==FALSE)], collapse=", ")))}


}

csum<-NULL
for (i in 1:dim(colonyfile$excluded.paternities)[1]){
csum[i]<-length(colonyfile$excluded.paternities[i,][!is.na(colonyfile$excluded.paternities[i,])])}
colonyfile$excluded.paternities[,1]<-csum
csum<-csum-1

write.table(paste(colonyfile$n.excluded.paternities,"!Number of offspring with known excluded paternity"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)


colonyfile$excluded.paternities<-cbind(as.character(colonyfile$excluded.paternities[,1]),csum,colonyfile$excluded.paternities[,2:dim(colonyfile$excluded.paternities)[2]])

colonyfile$excluded.paternities[,1+dim(colonyfile$excluded.paternities)[2]]<-c("!Offspring ID, number of excluded males, the IDs of excluded males",rep("",dim(colonyfile$excluded.paternities)[1]-1))

write.table(colonyfile$excluded.paternities,name,append=TRUE,quote=FALSE,na=" ",row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)

}else{
#If there are no excluded paternities
write.table(paste(colonyfile$n.excluded.paternities," !Number of offspring with known excluded paternity"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)
}

#######################################################
#Define excluded MATERNITIES
#######################################################
 
while(length(colonyfile$n.excluded.maternities)==0){
cat("Enter the number of offspring with known excluded MATERNITY.\n\n\n")
colonyfile$n.excluded.maternities<-as.numeric(scan(n=1,what="integer"))

if(length(colonyfile$n.excluded.maternities)!=0){
#Whole number warning 
if(is.whole(colonyfile$n.excluded.maternities)==FALSE){
flush.console()
colonyfile<-colonyfile[which(names(colonyfile)!="n.excluded.maternities")];
warning("The number of excluded maternities must be a whole number!\n",immediate.=TRUE)}}
}

if(colonyfile$n.excluded.maternities>0){


#Get the path, and delimiter, to the file...
while(length(colonyfile$excluded.maternities.PATH)==0){
	cat("Provide the path to the excluded MATERNITY file.\n\n\n")
	flush.console()
	colonyfile$excluded.maternities.PATH<-file.choose()

#	cat("What is the delimiter for this file?\n\n\n")
#	flush.console()
#	switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,cat("Nothing done\n\n\n"), colonyfile$delim.for.excluded.maternities.PATH<-"", colonyfile$delim.for.excluded.maternities.PATH<-"\t", colonyfile$delim.for.excluded.maternities.PATH<-",",delim.for.excluded.maternities.PATH<-"Other")
#
#		#Caveat for if the delimiter is OTHER
#		while(length(colonyfile$delim.for.excluded.maternities.PATH)=="Other"){
#		if(colonyfile$delim.for.excluded.maternities.PATH=="Other"){
#		cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
#		colonyfile$delim.for.excluded.maternities.PATH<-scan(n=1,what="character")}}
colonyfile$delim.for.excluded.maternities.PATH<-""

#Read in the data...
colonyfile$excluded.maternities<-read.table(colonyfile$excluded.maternities.PATH,header=FALSE,sep=colonyfile$delim.for.excluded.maternities.PATH,colClasses=c("character"),fill=TRUE,flush=TRUE,na.strings="")

#Check the data
if(colonyfile$n.excluded.maternities!=dim(colonyfile$excluded.maternities)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="excluded.maternities.PATH")];
flush.console();
warning(paste("The number of defined excluded maternities ","(", colonyfile$n.excluded.maternities,") does not equal the number provided in the file selected (", dim(colonyfile$excluded.maternities)[1],").\n\n",sep=""),immediate.=TRUE)
}

if(colonyfile$n.excluded.maternities>0){#Write ExcludedMaternity.txt file
temp1<-as.data.frame(colonyfile$excluded.maternities)
names(temp1)<-c("OffspringID",paste("ExcludedMotherID",1:(dim(temp1)[2]-1),sep=""))
write.table(temp1,"ExcludedMaternity.txt",row.names=FALSE,quote=FALSE,col.names=TRUE)}

#Futher checks
#if this is true, then all offspring in the diad file are present in the offspring genotype file
if(sum(colonyfile$excluded.maternities$V1%in%colonyfile$Offspring[,1])==length(colonyfile$excluded.maternities$V1)){}else{
colonyfile<-colonyfile[which(names(colonyfile)!="excluded.maternities.PATH")];
flush.console();
warning(paste("Offspring in excluded maternities file are not present in the offspring genotype data:",paste(colonyfile$excluded.maternities$V1[which(colonyfile$excluded.maternities$V1%in%colonyfile$mothers[,1]==FALSE)], collapse=", ")))
}

os<-na.omit(as.vector(as.matrix(colonyfile$excluded.maternities[,2:dim(colonyfile$excluded.maternities)[2]])))

if(sum(os%in%colonyfile$mothers[,1])==length(os)){}else{
colonyfile<-colonyfile[which(names(colonyfile)!="excluded.maternities.PATH")];
flush.console();
warning(paste("Fathers in excluded maternities file are not present in the mothers genotype data:",paste(os[which(os%in%colonyfile$mothers[,1]==FALSE)], collapse=", ")))}



}

csum<-NULL
for (i in 1:dim(colonyfile$excluded.maternities)[1]){
csum[i]<-length(colonyfile$excluded.maternities[i,][!is.na(colonyfile$excluded.maternities[i,])])}
colonyfile$excluded.maternities[,1]<-csum
csum<-csum-1

write.table(paste(colonyfile$n.excluded.maternities,"!Number of offspring with known excluded maternity"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)

colonyfile$excluded.maternities<-cbind(as.character(colonyfile$excluded.maternities[,1]),csum,colonyfile$excluded.maternities[,2:dim(colonyfile$excluded.maternities)[2]])

colonyfile$excluded.maternities[,1+dim(colonyfile$excluded.maternities)[2]]<-c("!Offspring ID, number of excluded females, the IDs of excluded females",rep("",dim(colonyfile$excluded.maternities)[1]-1))

write.table(colonyfile$excluded.maternities,name,append=TRUE,quote=FALSE,na=" ",row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)

}else{
#If there are no excluded maternities
write.table(paste(colonyfile$n.excluded.maternities," !Number of offspring with known excluded maternity"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)
}


#######################################################
#Define EXCLUDED PATERNAL sibships
#######################################################
while(length(colonyfile$n.excluded.paternal.sibships)==0){
cat("Enter the number of offspring with known excluded PATERNAL sibships.\n\n\n")
colonyfile$n.excluded.paternal.sibships<-as.numeric(scan(n=1,what="integer"))

if(length(colonyfile$n.excluded.paternal.sibships)!=0){
#Whole number warning 
if(is.whole(colonyfile$n.excluded.paternal.sibships)==FALSE){
flush.console()
colonyfile<-colonyfile[which(names(colonyfile)!="n.excluded.paternal.sibships")];
warning("The number of excluded paternal sibships must be a whole number!\n",immediate.=TRUE)}}
}

if(colonyfile$n.excluded.paternal.sibships>0){

#Get the path, and delimiter, to the file...
while(length(colonyfile$excluded.paternal.sibships.PATH)==0){
	cat("Provide the path to the excluded PATERNAL sibships file.\n\n\n")
	flush.console()
	colonyfile$excluded.paternal.sibships.PATH<-file.choose()

#	cat("What is the delimiter for this file?\n\n\n")
#	flush.console()
#	switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,cat("Nothing done\n\n\n"), colonyfile$delim.for.excluded.paternal.sibships.PATH<-"", colonyfile$delim.for.excluded.paternal.sibships.PATH<-"\t", colonyfile$delim.for.excluded.paternal.sibships.PATH<-",",delim.for.excluded.paternal.sibships.PATH<-"Other")
#
#		#Caveat for if the delimiter is OTHER
#		while(length(colonyfile$delim.for.excluded.paternal.sibships.PATH)=="Other"){
#		if(colonyfile$delim.for.excluded.paternal.sibships.PATH=="Other"){
#		cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
#		colonyfile$delim.for.excluded.paternal.sibships.PATH<-scan(n=1,what="character")}}

colonyfile$delim.for.excluded.paternal.sibships.PATH<-""

#Read in the data...
colonyfile$excluded.paternal.sibships<-read.table(colonyfile$excluded.paternal.sibships.PATH,header=FALSE,sep=colonyfile$delim.for.excluded.paternal.sibships.PATH,colClasses=c("character"),fill=TRUE,flush=TRUE,na.strings="")

#Check the data
if(colonyfile$n.excluded.paternal.sibships!=dim(colonyfile$excluded.paternal.sibships)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="excluded.paternal.sibships.PATH")];
flush.console();
warning(paste("The number of defined excluded paternal sibships ","(", colonyfile$n.excluded.paternal.sibships,") does not equal the number provided in the file selected (", dim(colonyfile$excluded.paternal.sibships)[1],").\n\n",sep=""),immediate.=TRUE)
}

#Further checks - do excluded sibs appear in offspring file
os<-na.omit(as.vector(as.matrix(colonyfile$excluded.paternal.sibships[,2:dim(colonyfile$excluded.paternal.sibships)[2]])))

if(sum(os%in%colonyfile$mothers[,1])==length(os)){}else{
colonyfile<-colonyfile[which(names(colonyfile)!="excluded.paternal.sibships.PATH")];
flush.console();
warning(paste("Offspring in excluded paternal sibships file are not present in the offspring genotype data:",paste(os[which(os%in%colonyfile$Offspring[,1]==FALSE)], collapse=", ")))}


}



#ORJ: The formatting for this needs to be checked with Jinliang.
colonyfile$excluded.paternal.sibships[,1+dim(colonyfile$excluded.paternal.sibships)[2]]<-c("!Size of known excluded paternal sibship, and IDs of excluded offspring in the sibship",rep("",dim(colonyfile$excluded.paternal.sibships)[1]-1))
csum<-NULL
for (i in 1:dim(colonyfile$excluded.paternal.sibships)[1]){
csum[i]<-length(colonyfile$excluded.paternal.sibships[i,][!is.na(colonyfile$excluded.paternal.sibships[i,])])}
csum<-csum-1

colonyfile$excluded.paternal.sibships<-cbind(csum,colonyfile$excluded.paternal.sibships)


write.table(paste(colonyfile$n.excluded.paternal.sibships,"!Number of offspring with known excluded paternal sibships"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)

write.table(colonyfile$known.maternities,name,append=TRUE,quote=FALSE,na=" ",row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)

}else{
#If there are no excluded maternities
write.table(paste(colonyfile$n.excluded.paternal.sibships," !Number of offspring with known excluded paternal sibships"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)
}


#######################################################
#Define EXCLUDED MATERNAL sibships
#######################################################

while(length(colonyfile$n.excluded.maternal.sibships)==0){
cat("Enter the number of offspring with known excluded MATERNAL sibships.\n\n\n")
colonyfile$n.excluded.maternal.sibships<-as.numeric(scan(n=1,what="integer"))

if(length(colonyfile$n.excluded.maternal.sibships)!=0){
#Whole number warning 
if(is.whole(colonyfile$n.excluded.maternal.sibships)==FALSE){
flush.console()
colonyfile<-colonyfile[which(names(colonyfile)!="n.excluded.maternal.sibships")];
warning("The number of excluded maternal sibships must be a whole number!\n",immediate.=TRUE)}}
}

if(colonyfile$n.excluded.maternal.sibships>0){


#Get the path, and delimiter, to the file...
while(length(colonyfile$excluded.maternal.sibships.PATH)==0){
	cat("Provide the path to the excluded MATERNAL sibships file.\n\n\n")
	flush.console()
	colonyfile$excluded.maternal.sibships.PATH<-file.choose()

#	cat("What is the delimiter for this file?\n\n\n")
#	flush.console()
#	switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,cat("Nothing done\n\n\n"), colonyfile$delim.for.excluded.maternal.sibships.PATH<-"", colonyfile$delim.for.excluded.maternal.sibships.PATH<-"\t", colonyfile$delim.for.excluded.maternal.sibships.PATH<-",",delim.for.excluded.maternal.sibships.PATH<-"Other")
#
#		#Caveat for if the delimiter is OTHER
#		while(length(colonyfile$delim.for.excluded.maternal.sibships.PATH)=="Other"){
#		if(colonyfile$delim.for.excluded.maternal.sibships.PATH=="Other"){
#		cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
#		colonyfile$delim.for.excluded.maternal.sibships.PATH<-scan(n=1,what="character")}}
colonyfile$delim.for.excluded.maternal.sibships.PATH<-""

#Read in the data...
colonyfile$excluded.maternal.sibships<-read.table(colonyfile$excluded.maternal.sibships.PATH,header=FALSE,sep=colonyfile$delim.for.excluded.maternal.sibships.PATH,colClasses=c("character"),fill=TRUE,flush=TRUE,na.strings="")

#Check the data
if(colonyfile$n.excluded.maternal.sibships!=dim(colonyfile$excluded.maternal.sibships)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="excluded.maternal.sibships.PATH")];
flush.console();
warning(paste("The number of defined excluded maternal sibships ","(", colonyfile$n.excluded.maternal.sibships,") does not equal the number provided in the file selected (", dim(colonyfile$excluded.maternal.sibships)[1],").\n\n",sep=""),immediate.=TRUE)
}

#Further checks - do excluded sibs appear in offspring file
os<-na.omit(as.vector(as.matrix(colonyfile$excluded.maternal.sibships[,2:dim(colonyfile$excluded.maternal.sibships)[2]])))

if(sum(os%in%colonyfile$mothers[,1])==length(os)){}else{
colonyfile<-colonyfile[which(names(colonyfile)!="excluded.maternal.sibships.PATH")];
flush.console();
warning(paste("Offspring in excluded maternal sibships file are not present in the offspring genotype data:",paste(os[which(os%in%colonyfile$Offspring[,1]==FALSE)], collapse=", ")))}

}

colonyfile$excluded.maternal.sibships[,1+dim(colonyfile$excluded.maternal.sibships)[2]]<-c("!Size of known excluded maternal sibship, and IDs of excluded offspring in the sibship",rep("",dim(colonyfile$excluded.maternal.sibships)[1]-1))
csum<-NULL
for (i in 1:dim(colonyfile$excluded.maternal.sibships)[1]){
csum[i]<-length(colonyfile$excluded.maternal.sibships[i,][!is.na(colonyfile$excluded.maternal.sibships[i,])])}
csum<-csum-1

colonyfile$excluded.maternal.sibships<-cbind(csum,colonyfile$excluded.maternal.sibships)


write.table(paste(colonyfile$n.excluded.maternal.sibships,"!Number of offspring with known excluded maternal sibships"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)

write.table(colonyfile$excluded.maternal.sibships,name,append=TRUE,quote=FALSE,na=" ",row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)

}else{
#If there are no excluded maternities
write.table(paste(colonyfile$n.excluded.maternal.sibships," !Number of offspring with known excluded maternal sibships"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)
}

#######################################################
#Other outputs
#######################################################

#MarkerTypeErrorRate.txt
temp1<-as.data.frame(colonyfile$Markers)
temp1<-temp1[,1:dim(temp1)[2]-1]
names(temp1)<-paste("Locus-",1:dim(temp1)[2],sep="")
write.table(temp1,"MarkerTypeErrorRate.txt",row.names=FALSE,quote=FALSE,col.names=TRUE)

#AlleleFrequency.txt 
if(colonyfile$knownAFreq==1){
temp1<-as.data.frame(colonyfile$allele.frequency)
temp1<-temp1[,1:dim(temp1)[2]-1]
names(temp1)<-paste(paste("Locus-",rep(1:(dim(temp1)[2]/2),each=2),sep=""),rep(1:2,(dim(temp1)[2]/2)),sep=".")
write.table(temp1,"MarkerTypeErrorRate.txt",row.names=FALSE,quote=FALSE,col.names=TRUE)}

#OffspringGenotype.txt
temp1<-as.data.frame(colonyfile$Offspring)
temp1<-temp1[,1:dim(temp1)[2]-1]
n<-(dim(temp1)[2])-1
names(temp1)<-c("Offspring",paste(paste("Marker",rep(1:(n/2),each=2),sep=""),rep(1:2,(n/2)),sep="-"))
write.table(temp1,"OffspringGenotype.txt",row.names=FALSE,quote=FALSE,col.names=TRUE)

#MaleGenotype.txt
temp1<-as.data.frame(colonyfile$fathers)
temp1<-temp1[,1:dim(temp1)[2]-1]
n<-(dim(temp1)[2])-1
names(temp1)<-c("Male",paste(paste("Marker",rep(1:(n/2),each=2),sep=""),rep(1:2,(n/2)),sep="-"))
write.table(temp1,"MaleGenotype.txt",row.names=FALSE,quote=FALSE,col.names=TRUE)

#FemaleGenotype.txt
temp1<-as.data.frame(colonyfile$mothers)
temp1<-temp1[,1:dim(temp1)[2]-1]
n<-(dim(temp1)[2])-1
names(temp1)<-c("Female",paste(paste("Marker",rep(1:(n/2),each=2),sep=""),rep(1:2,(n/2)),sep="-"))
write.table(temp1,"FemaleGenotype.txt",row.names=FALSE,quote=FALSE,col.names=TRUE)

#KnownPaternalDiads.txt
if(colonyfile$n.known.paternal.diads>0){
temp1<-as.data.frame(colonyfile$known.paternal.diads)
temp1<-temp1[,1:dim(temp1)[2]-1]
names(temp1)<-c("OffspringID","Father")
write.table(temp1,"KnownPaternalDiads.txt",row.names=FALSE,quote=FALSE,col.names=TRUE)}

#KnownMaternalDiads.txt
if(colonyfile$n.known.maternal.diads>0){
temp1<-as.data.frame(colonyfile$known.maternal.diads)
temp1<-temp1[,1:dim(temp1)[2]-1]
names(temp1)<-c("Offspring","Mother")
write.table(temp1,"KnownMaternalDiads.txt",row.names=FALSE,quote=FALSE,col.names=TRUE)}


#KnownPaternity.txt - see earlier
#KnownMaternity.txt - see earlier

#ExcludedPaternity.txt - see earlier
#ExcludedMaternity.txt - see earlier

#Produce summary information text file.
write.table(paste("Output file path & name : ",wd,name,"\n",
"Number of loci : ", colonyfile$n.loci,"\n",
"Number of offspring in the sample : ", colonyfile$n.offspring,"\n",
"Number of male candidates : ", colonyfile$n.father,"\n",
"Number of female candidates : ", colonyfile$n.mother,"\n",
"Number of known paternal sibships : ", colonyfile$n.paternal.sibs.or.paternities,"\n",
"Number of known maternal sibships : ", colonyfile$n.maternal.sibs.or.maternities,"\n",
"Number of offspring with excluded fathers : ", colonyfile$n.excluded.paternities,"\n",
"Number of offspring with excluded mothers : ", colonyfile$n.excluded.maternities,"\n",
"Male mating system : ", if(colonyfile$malepolygamy==1){"Polygamous"}else{"Monogamous"},"\n",
"Female mating system : ", if(colonyfile$femalepolygamy==1){"Polygamous"}else{"Monogamous"},"\n",
"Number of threads : ", "nA","\n",
"Number of Excluded Paternal Sibships : ", colonyfile$n.excluded.paternal.sibships,"\n",
"Number of Excluded Maternal Sibships : ", colonyfile$n.excluded.paternal.sibships,"\n",
"Seed for random number generator : ", colonyfile$rseed,"\n",
"Allele frequency : ", if(colonyfile$updateallelefreq==1){"Updating by accounting for the inferred relationship"}else{"No updating by accounting for the inferred relationship"},"\n",
"Species : ", if(colonyfile$ploidy==1){"HaploDiploid"}else{"Diploid"},"\n",
"Known population allele frequency : ", if(colonyfile$knownAFreq==1){"Yes"}else{"No"},"\n",
"Number of run : ", colonyfile$n.runs,"\n",
"Length of run : ", if(colonyfile$runlength==1){"Small"}else{if(colonyfile$runlength==2){"Medium"}else{"Long"}},"\n",
"Monitor intermiediate results by : ", if(colonyfile$monitortype==1){paste("Every",colonyfile$interval,"seconds")}else{paste("Every",colonyfile$interval,"iterations")},"\n",
"Project data input produced : ", date(),"\n",
"NOTE to the Project: ", colonyfile$note,"\n",sep=""),"ProjectInformation.txt",row.names=FALSE,col.names=FALSE,quote=FALSE)



cat("Finished!")
cat(paste("Your file is called",name,"and is placed in",wd,"...\n\n\n"))

#This could be useful at some point.
return(colonyfile)
}    