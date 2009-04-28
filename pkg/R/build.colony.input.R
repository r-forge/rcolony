build.colony.input<-function(wd=getwd(),name="Colony2.DAT"){

colonyfile<-NULL

cat("This function will construct a Colony input file.\n\n\n")
cat(paste("It will be called",name,"and be placed in",wd,"...\n\n\n"))

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
#  ! I, Number of offspring in the sample
#######################################################
while(length(colonyfile$n.offspring)==0){
cat("Enter number of offspring in the sample.\n\n\n")
colonyfile$n.offspring<-scan(n=1,what="integer")
write(paste(colonyfile$n.offspring,"! I, Number of offspring in the sample"),name,append=TRUE)}

#######################################################
#  ! I, Number of loci
#######################################################
while(length(colonyfile$n.loci)==0){
cat("Enter number of loci.\n\n\n")
colonyfile$n.loci<-scan(n=1,what="integer")
write(paste(colonyfile$n.loci,"! I, Number of loci"),name,append=TRUE)}

#######################################################
#  ! I, Seed for random number generator
#######################################################
while(length(colonyfile$rseed)==0){
cat("Enter seed for random number generator.\n\n\n")
colonyfile$rseed<-scan(n=1,what="integer")
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
cat("What kind of species is it?\n\n\n")
switch(menu(c("Diploid species", "HaploDiploid species")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$ploidy<-0, colonyfile$ploidy<-1)
write(paste(colonyfile$ploidy,"! B, 0/1=Diploid species/HaploDiploid species"),name,append=TRUE)

#######################################################
#  ! B, 0/1=Polygamy/Monogamy for males & females
#######################################################
cat("Are males monogamous or polygamous?\n\n\n")
switch(menu(c("Males monogamous", "Males polygamous")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$malepolygamy<-0, colonyfile$malepolygamy<-1)

cat("Are females monogamous or polygamous?\n\n\n")
switch(menu(c("Females monogamous", "Females polygamous")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$femalepolygamy<-0, colonyfile$femalepolygamy<-1)
write(paste(colonyfile$malepolygamy,colonyfile$femalepolygamy,"! B, 0/1=Polygamy/Monogamy for males & females"),name,append=TRUE)

#######################################################
#  ! B,R,R : Use sibship prior, Y/N=1/0. If Yes, give mean paternal, maternal sibship size
#######################################################
cat("Use sibship prior?\n\n\n")
switch(menu(c("YES", "NO")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$sibship.prior<-1, colonyfile$sibship.prior<-0)

if(colonyfile$sibship.prior==0){
	colonyfile$sibship.prior.paternal<-0
	colonyfile$sibship.prior.maternal<-0
write(paste(colonyfile$sibship.prior,colonyfile$sibship.prior.paternal,colonyfile$sibship.prior.maternal,"! B,R,R : Use sibship prior, Y/N=1/0. If Yes, give mean paternal, maternal sibship size"),name,append=TRUE)
	}else{

while(length(colonyfile$sibship.prior.paternal)==0){
cat("Enter the paternal sibship size.\n\n\n")
colonyfile$sibship.prior.paternal<-scan(n=1,what="integer")}

while(length(colonyfile$sibship.prior.maternal)==0){
cat("Enter the maternal sibship size.\n\n\n")
colonyfile$sibship.prior.maternal<-scan(n=1,what="integer")}
write(paste(colonyfile$sibship.prior,colonyfile$sibship.prior.paternal,colonyfile$sibship.prior.maternal,"! B,R,R : Use sibship prior, Y/N=1/0. If Yes, give mean paternal, maternal sibship size"),name,append=TRUE)
}

#######################################################
#  ! B, 0/1=Unknown/Known population allele frequency
#######################################################
cat("Unknown/Known population allele frequency?\n\n\n")
switch(menu(c("Unknown", "Known")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$knownAFreq<-0, colonyfile$knownAFreq<-1)
write(paste(colonyfile$knownAFreq,"! B, 0/1=Unknown/Known population allele frequency"),name,append=TRUE)

#######################################################
#  ! I, Number of runs
#######################################################
while(length(colonyfile$n.runs)==0){
cat("Number of runs.\n\n\n")
colonyfile$n.runs<-scan(n=1,what="integer")
write(paste(colonyfile$n.runs,"! I, Number of runs"),name,append=TRUE)}

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
colonyfile$interval<-scan(n=1,what="integer")
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

write("\n\n",name,append=TRUE)

#######################################################
#Marker file import
#######################################################

#Give the path to the marker types and error rate file. This should be a file with a number of columns equal to the number of markers used.
#There should be 4 rows, 1) marker ID, 2) marker type, 3) marker specific allelic dropout rate, 4) marker specific other typing error rate.

while(length(colonyfile$MarkerPATH)==0){
cat("Provide the path to the Marker Types and Error Rate file.\n\n\n");Sys.sleep(.5)
flush.console()
colonyfile$MarkerPATH<-file.choose()

cat("What is the delimiter for this file?\n\n\n")
flush.console()
switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$delim.for.markers<-"", colonyfile$delim.for.markers<-"\t", colonyfile$delim.for.markers<-",",delim.for.markers<-"Other")

while(length(colonyfile$delim.for.markers)=="Other"){
if(colonyfile$delim.for.markers=="Other"){
cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
colonyfile$delim.for.markers<-scan(n=1,what="character")}}

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


cat("What is the delimiter for this file?\n\n\n")
flush.console()
switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$delim.for.OSGenotype<-"", colonyfile$delim.for.OSGenotype<-"\t", colonyfile$delim.for.OSGenotype<-",",delim.for.OSGenotype<-"Other")

while(length(colonyfile$delim.for.OSGenotype)=="Other"){
if(colonyfile$delim.for.OSGenotype=="Other"){
cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
colonyfile$delim.for.OSGenotype<-scan(n=1,what="character")}}

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
while(length(colonyfile$dadprob)==0){
cat("What is the probability that the FATHER of an offpring is included in the candidate set?\n\n\n E.g. 0.5\n\n\n")
colonyfile$dadprob<-scan(n=1,what="integer")
if(colonyfile$dadprob>1){
flush.console()
cat("Probabilities must be less than or equal to 1.\n")
colonyfile<-colonyfile[which(names(colonyfile)!="dadprob")]}
}

#######################################################
#Number of candidate dads
#######################################################
while(length(colonyfile$n.dad)==0){
cat("How many candidate FATHERS are there?\n\n\n")
colonyfile$n.dad<-scan(n=1,what="integer")}

#######################################################
#Import candidate FATHERS file
#######################################################
while(length(colonyfile$dadsPATH)==0){
cat("Provide the path to the candidate FATHERS file.\n\n\n")
flush.console()
colonyfile$dadsPATH<-file.choose()

cat("What is the delimiter for this file?\n\n\n")
flush.console()
switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$delim.for.dads<-"", colonyfile$delim.for.dads<-"\t", colonyfile$delim.for.dads<-",",delim.for.dads<-"Other")

while(length(colonyfile$delim.for.dads)=="Other"){
if(colonyfile$delim.for.dads=="Other"){
cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
colonyfile$delim.for.dads<-scan(n=1,what="character")}}


colonyfile$dads<-read.table(colonyfile$dadsPATH,header=FALSE,sep=colonyfile$delim.for.dads,colClasses=c("character"))
if(colonyfile$n.dad!=dim(colonyfile$dads)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="dadsPATH")];
flush.console();
warning(paste("The number of defined DADS ","(", colonyfile$n.dad,") does not equal the number of DADS provided in the file selected (", dim(colonyfile$dads)[1],").\n\n",sep=""),immediate.=TRUE)}
}

#######################################################
#MOTHERS - probability of inclusion in candidate set
#######################################################
while(length(colonyfile$mumprob)==0){
cat("What is the probability that the MOTHER of an offpring is included in the candidate set?\n\n\n E.g. 0.5\n\n\n")
colonyfile$mumprob<-scan(n=1,what="integer")
if(colonyfile$mumprob>1){
flush.console()
cat("Probabilities must be less than or equal to 1.\n")
colonyfile<-colonyfile[which(names(colonyfile)!="mumprob")]}
}

#######################################################
#Number of candidate mothers
#######################################################
while(length(colonyfile$n.mum)==0){
cat("How many candidate MOTHERS are there?\n\n\n")
colonyfile$n.mum<-scan(n=1,what="integer")}

#######################################################
#Import candidate MOTHERS
#######################################################
while(length(colonyfile$mumsPATH)==0){
cat("Provide the path to the candidate MOTHERS file.\n\n\n")
flush.console()
colonyfile$mumsPATH<-file.choose()

flush.console()
switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,
       cat("Nothing done\n\n\n"), colonyfile$delim.for.mums<-"", colonyfile$delim.for.mums<-"\t", colonyfile$delim.for.mums<-",",delim.for.mums<-"Other")

while(length(colonyfile$delim.for.mums)=="Other"){
if(colonyfile$delim.for.mums=="Other"){
cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
colonyfile$delim.for.mums<-scan(n=1,what="character")}}

colonyfile$mums<-read.table(colonyfile$mumsPATH,header=FALSE,sep=colonyfile$delim.for.mums,colClasses=c("character")) 
if(colonyfile$n.mum!=dim(colonyfile$mums)[1]){colonyfile<-colonyfile[which(names(colonyfile)!="mumsPATH")];
flush.console();
warning(paste("The number of defined MUMS ","(", colonyfile$n.mum,") does not equal the number of MUMS provided in the file selected (", dim(colonyfile$mums)[1],").\n\n",sep=""),immediate.=TRUE)}
}

write(paste(colonyfile$dadprob,colonyfile$mumprob,"!Probabilities that the dad and mum of an offspring included in candidates"),name,append=TRUE)
write(paste(colonyfile$n.dad,colonyfile$n.mum,"!Numbers of candidate males and females"),name,append=TRUE)
write("",name,append=TRUE)

colonyfile$dads[,1+dim(colonyfile$dads)[2]]<-c("!Candidate M ID and genotypes",rep("",dim(colonyfile$dads)[1]-1))
write.table(colonyfile$dads,name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)

colonyfile$mums[,1+dim(colonyfile$mums)[2]]<-c("!Candidate F ID and genotypes",rep("",dim(colonyfile$mums)[1]-1))
write.table(colonyfile$mums,name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)


#######################################################
#Define known PATERNAL diads
#######################################################
cat("Enter the number of known PATERNAL-OFFSPRING diads.\n\n\n")
colonyfile$n.known.paternal.diads<-scan(n=1,what="integer")

if(colonyfile$n.known.paternal.diads>0){

#If there are some known paternal diads...

#Get the path, and delimiter, to the file...
while(length(colonyfile$paternal.diads.PATH)==0){
	cat("Provide the path to the PATERNAL diads file.\n\n\n")
	flush.console()
	colonyfile$paternal.diads.PATH<-file.choose()

	cat("What is the delimiter for this file?\n\n\n")
	flush.console()
	switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,cat("Nothing done\n\n\n"), colonyfile$delim.for.paternal.sibs.PATH<-"", colonyfile$delim.for.paternal.sibs.PATH<-"\t", colonyfile$delim.for.paternal.sibs.PATH<-",",delim.for.paternal.sibs.PATH<-"Other")

		#Caveat for if the delimiter is OTHER
		while(length(colonyfile$delim.for.paternal.diads.PATH)=="Other"){
		if(colonyfile$delim.for.paternal.diads.PATH=="Other"){
		cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
		colonyfile$delim.for.paternal.diads.PATH<-scan(n=1,what="character")}}

#Read in the data...
colonyfile$known.paternal.diads<-read.table(colonyfile$paternal.diads.PATH,header=FALSE,sep=colonyfile$delim.for.paternal.diads.PATH,colClasses=c("character"))

#Check the data
if(colonyfile$n.known.paternal.diads!=dim(colonyfile$known.paternal.diads)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="paternal.diads.PATH")];
flush.console();
warning(paste("The number of defined paternal diads ","(", colonyfile$n.paternal.sibs.or.paternities,") does not equal the number of paternal diads provided in the file selected (", dim(colonyfile$known.paternal.diads)[1],").\n\n",sep=""),immediate.=TRUE)
}}


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

cat("Enter the number of known MATERNAL-OFFSPRING diads.\n\n\n")
colonyfile$n.known.maternal.diads<-scan(n=1,what="integer")

if(colonyfile$n.known.maternal.diads>0){

#If there are some known maternal diads...

#Get the path, and delimiter, to the file...
while(length(colonyfile$maternal.diads.PATH)==0){
	cat("Provide the path to the MATERNAL diads file.\n\n\n")
	flush.console()
	colonyfile$maternal.diads.PATH<-file.choose()

	cat("What is the delimiter for this file?\n\n\n")
	flush.console()
	switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,cat("Nothing done\n\n\n"), colonyfile$delim.for.maternal.sibs.PATH<-"", colonyfile$delim.for.maternal.sibs.PATH<-"\t", colonyfile$delim.for.maternal.sibs.PATH<-",",delim.for.maternal.sibs.PATH<-"Other")

		#Caveat for if the delimiter is OTHER
		while(length(colonyfile$delim.for.maternal.diads.PATH)=="Other"){
		if(colonyfile$delim.for.maternal.diads.PATH=="Other"){
		cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
		colonyfile$delim.for.maternal.diads.PATH<-scan(n=1,what="character")}}

#Read in the data...
colonyfile$known.maternal.diads<-read.table(colonyfile$maternal.diads.PATH,header=FALSE,sep=colonyfile$delim.for.maternal.diads.PATH,colClasses=c("character"))

#Check the data
if(colonyfile$n.known.maternal.diads!=dim(colonyfile$known.maternal.diads)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="maternal.diads.PATH")];
flush.console();
warning(paste("The number of defined maternal diads ","(", colonyfile$n.known.maternal.diads,") does not equal the number of maternal diads provided in the file selected (", dim(colonyfile$known.maternal.diads)[1],").\n\n",sep=""),immediate.=TRUE)
}}


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
cat("Enter the number of known PATERNAL sibship/paternity.\n\n\n")
colonyfile$n.paternal.sibs.or.paternities<-scan(n=1,what="integer")

if(colonyfile$n.paternal.sibs.or.paternities>0){

#If there are some known sibships...

#Get the path, and delimiter, to the file...
while(length(colonyfile$paternal.sibs.PATH)==0){
	cat("Provide the path to the candidate FATHERS file.\n\n\n")
	flush.console()
	colonyfile$paternal.sibs.PATH<-file.choose()

	cat("What is the delimiter for this file?\n\n\n")
	flush.console()
	switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,cat("Nothing done\n\n\n"), colonyfile$delim.for.paternal.sibs.PATH<-"", colonyfile$delim.for.paternal.sibs.PATH<-"\t", colonyfile$delim.for.paternal.sibs.PATH<-",",delim.for.paternal.sibs.PATH<-"Other")

		#Caveat for if the delimiter is OTHER
		while(length(colonyfile$delim.for.paternal.sibs.PATH)=="Other"){
		if(colonyfile$delim.for.paternal.sibs.PATH=="Other"){
		cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
		colonyfile$delim.for.paternal.sibs.PATH<-scan(n=1,what="character")}}

#Read in the data...
colonyfile$known.paternities<-read.table(colonyfile$paternal.sibs.PATH,header=FALSE,sep=colonyfile$delim.for.paternal.sibs.PATH,colClasses=c("character"))

#Check the data
if(colonyfile$n.paternal.sibs.or.paternities!=dim(colonyfile$known.paternities)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="paternal.sibs.PATH")];
flush.console();
warning(paste("The number of defined paternal sibs/paternities ","(", colonyfile$n.paternal.sibs.or.paternities,") does not equal the number of paternities provided in the file selected (", dim(colonyfile$known.paternities)[1],").\n\n",sep=""),immediate.=TRUE)
}}


write.table(paste(colonyfile$n.paternal.sibs.or.paternities," !Number of known paternal sibships"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
colonyfile$known.paternities[,1+dim(colonyfile$known.paternities)[2]]<-c("!Size of known paternal sibship, and IDs of offspring in the sibship",rep("",dim(colonyfile$known.paternities)[1]-1))
csum<-NULL
for (i in 1:dim(colonyfile$known.paternities)[1]){
csum[i]<-length(colonyfile$known.paternities[i,][!is.na(colonyfile$known.paternities[i,])])}
rownames(colonyfile$known.paternities)<-csum

write.table(colonyfile$known.paternities,name,append=TRUE,quote=FALSE,na=" ",row.names=TRUE,col.names=FALSE)
write("",name,append=TRUE)

}else{
#If there are no known sibships
write.table(paste(colonyfile$n.paternal.sibs.or.paternities," !Number of known paternal sibships"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)
}

#######################################################
#Define MATERNAL sibships
#######################################################
cat("Enter the number of known MATERNAL sibship/maternity.\n\n\n")
colonyfile$n.maternal.sibs.or.maternities<-scan(n=1,what="integer")

if(colonyfile$n.maternal.sibs.or.maternities>0){

#If there are some known sibships...

#Get the path, and delimiter, to the file...
while(length(colonyfile$maternal.sibs.PATH)==0){
	cat("Provide the path to the candidate MOTHERS file.\n\n\n")
	flush.console()
	colonyfile$maternal.sibs.PATH<-file.choose()

	cat("What is the delimiter for this file?\n\n\n")
	flush.console()
	switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,cat("Nothing done\n\n\n"), colonyfile$delim.for.maternal.sibs.PATH<-"", colonyfile$delim.for.maternal.sibs.PATH<-"\t", colonyfile$delim.for.maternal.sibs.PATH<-",",delim.for.maternal.sibs.PATH<-"Other")

		#Caveat for if the delimiter is OTHER
		while(length(colonyfile$delim.for.maternal.sibs.PATH)=="Other"){
		if(colonyfile$delim.for.maternal.sibs.PATH=="Other"){
		cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
		colonyfile$delim.for.maternal.sibs.PATH<-scan(n=1,what="character")}}

#Read in the data...
colonyfile$known.maternities<-read.table(colonyfile$maternal.sibs.PATH,header=FALSE,sep=colonyfile$delim.for.maternal.sibs.PATH,colClasses=c("character"))

#Check the data
if(colonyfile$n.maternal.sibs.or.maternities!=dim(colonyfile$known.maternities)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="maternal.sibs.PATH")];
flush.console();
warning(paste("The number of defined maternal sibs/maternities ","(", colonyfile$n.maternal.sibs.or.maternities,") does not equal the number of maternities provided in the file selected (", dim(colonyfile$known.maternities)[1],").\n\n",sep=""),immediate.=TRUE)
}}


write.table(paste(colonyfile$n.maternal.sibs.or.maternities," !Number of known maternal sibships"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)

colonyfile$known.maternities[,1+dim(colonyfile$known.maternities)[2]]<-c("!Size of known maternal sibship, and IDs of offspring in the sibship",rep("",dim(colonyfile$known.maternities)[1]-1))
csum<-NULL
for (i in 1:dim(colonyfile$known.maternities)[1]){
csum[i]<-length(colonyfile$known.maternities[i,][!is.na(colonyfile$known.maternities[i,])])}
rownames(colonyfile$known.maternities)<-csum

write.table(colonyfile$known.maternities,name,append=TRUE,quote=FALSE,na=" ",row.names=TRUE,col.names=FALSE)
write("",name,append=TRUE)

}else{
#If there are no known sibships
write.table(paste(colonyfile$n.maternal.sibs.or.maternities," !Number of known maternal sibships"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)
}


#######################################################
#Define excluded fathers
#######################################################
 
cat("Enter the number of offspring with known excluded paternity.\n\n\n")
colonyfile$n.excluded.paternity<-scan(n=1,what="integer")

if(colonyfile$n.excluded.paternity>0){


#Get the path, and delimiter, to the file...
while(length(colonyfile$excluded.paternity.PATH)==0){
	cat("Provide the path to the excluded PATERNITY file.\n\n\n")
	flush.console()
	colonyfile$excluded.paternity.PATH<-file.choose()

	cat("What is the delimiter for this file?\n\n\n")
	flush.console()
	switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,cat("Nothing done\n\n\n"), colonyfile$delim.for.excluded.paternity.PATH<-"", colonyfile$delim.for.excluded.paternity.PATH<-"\t", colonyfile$delim.for.excluded.paternity.PATH<-",",delim.for.excluded.paternity.PATH<-"Other")

		#Caveat for if the delimiter is OTHER
		while(length(colonyfile$delim.for.excluded.paternity.PATH)=="Other"){
		if(colonyfile$delim.for.excluded.paternity.PATH=="Other"){
		cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
		colonyfile$delim.for.excluded.paternity.PATH<-scan(n=1,what="character")}}

#Read in the data...
colonyfile$excluded.paternities<-read.table(colonyfile$excluded.paternity.PATH,header=FALSE,sep=colonyfile$delim.for.excluded.paternity.PATH,colClasses=c("character"))

#Check the data
if(colonyfile$n.excluded.paternity!=dim(colonyfile$excluded.paternities)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="excluded.paternity.PATH")];
flush.console();
warning(paste("The number of defined excluded paternities ","(", colonyfile$n.excluded.paternity,") does not equal the number provided in the file selected (", dim(colonyfile$excluded.paternities)[1],").\n\n",sep=""),immediate.=TRUE)
}}


#Make rownames, the offspring name. Then replace col 1 (Offspring ID
rownames(colonyfile$excluded.paternities)<-colonyfile$excluded.paternities[,1]

csum<-NULL
for (i in 1:dim(colonyfile$excluded.paternities)[1]){
csum[i]<-length(colonyfile$excluded.paternities[i,][!is.na(colonyfile$excluded.paternities[i,])])}
colonyfile$excluded.paternities[,1]<-csum

write.table(paste(colonyfile$n.excluded.paternities,"!Number of offspring with known excluded paternity"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)

colonyfile$excluded.paternities[,1+dim(colonyfile$excluded.paternities)[2]]<-c("!Offspring ID, number of excluded males, the IDs of excluded males",rep("",dim(colonyfile$excluded.paternities)[1]-1))

write.table(colonyfile$known.paternities,name,append=TRUE,quote=FALSE,na=" ",row.names=TRUE,col.names=FALSE)
write("",name,append=TRUE)

}else{
#If there are no excluded paternities
write.table(paste(colonyfile$n.excluded.paternities," !Number of offspring with known excluded paternity"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)
}

#######################################################
#Define excluded MOTHERS
#######################################################
 
cat("Enter the number of offspring with known excluded MATERNITY.\n\n\n")
colonyfile$n.excluded.maternity<-scan(n=1,what="integer")

if(colonyfile$n.excluded.maternity>0){


#Get the path, and delimiter, to the file...
while(length(colonyfile$excluded.maternity.PATH)==0){
	cat("Provide the path to the excluded MATERNITY file.\n\n\n")
	flush.console()
	colonyfile$excluded.maternity.PATH<-file.choose()

	cat("What is the delimiter for this file?\n\n\n")
	flush.console()
	switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,cat("Nothing done\n\n\n"), colonyfile$delim.for.excluded.maternity.PATH<-"", colonyfile$delim.for.excluded.maternity.PATH<-"\t", colonyfile$delim.for.excluded.maternity.PATH<-",",delim.for.excluded.maternity.PATH<-"Other")

		#Caveat for if the delimiter is OTHER
		while(length(colonyfile$delim.for.excluded.maternity.PATH)=="Other"){
		if(colonyfile$delim.for.excluded.maternity.PATH=="Other"){
		cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
		colonyfile$delim.for.excluded.maternity.PATH<-scan(n=1,what="character")}}

#Read in the data...
colonyfile$excluded.maternities<-read.table(colonyfile$excluded.maternity.PATH,header=FALSE,sep=colonyfile$delim.for.excluded.maternity.PATH,colClasses=c("character"))

#Check the data
if(colonyfile$n.excluded.maternity!=dim(colonyfile$excluded.maternities)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="excluded.maternity.PATH")];
flush.console();
warning(paste("The number of defined excluded maternities ","(", colonyfile$n.excluded.maternity,") does not equal the number provided in the file selected (", dim(colonyfile$excluded.maternities)[1],").\n\n",sep=""),immediate.=TRUE)
}}


#Make rownames, the offspring name. Then replace col 1 (Offspring ID
rownames(colonyfile$excluded.maternities)<-colonyfile$excluded.maternities[,1]

csum<-NULL
for (i in 1:dim(colonyfile$excluded.maternities)[1]){
csum[i]<-length(colonyfile$excluded.maternities[i,][!is.na(colonyfile$excluded.maternities[i,])])}
colonyfile$excluded.maternities[,1]<-csum

write.table(paste(colonyfile$n.excluded.maternities,"!Number of offspring with known excluded maternity"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)

colonyfile$excluded.maternities[,1+dim(colonyfile$excluded.maternities)[2]]<-c("!Offspring ID, number of excluded females, the IDs of excluded females",rep("",dim(colonyfile$excluded.maternities)[1]-1))

write.table(colonyfile$known.maternities,name,append=TRUE,quote=FALSE,na=" ",row.names=TRUE,col.names=FALSE)
write("",name,append=TRUE)

}else{
#If there are no excluded maternities
write.table(paste(colonyfile$n.excluded.maternities," !Number of offspring with known excluded maternity"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)
}


#######################################################
#Define EXCLUDED PATERNAL sibships
#######################################################
cat("Enter the number of offspring with known excluded paternal sibships.\n\n\n")
colonyfile$n.excluded.paternal.sibships<-scan(n=1,what="integer")

if(colonyfile$n.excluded.paternal.sibships>0){


#Get the path, and delimiter, to the file...
while(length(colonyfile$excluded.paternal.sibships.PATH)==0){
	cat("Provide the path to the excluded PATERNAL sibships file.\n\n\n")
	flush.console()
	colonyfile$excluded.paternal.sibships.PATH<-file.choose()

	cat("What is the delimiter for this file?\n\n\n")
	flush.console()
	switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,cat("Nothing done\n\n\n"), colonyfile$delim.for.excluded.paternal.sibships.PATH<-"", colonyfile$delim.for.excluded.paternal.sibships.PATH<-"\t", colonyfile$delim.for.excluded.paternal.sibships.PATH<-",",delim.for.excluded.paternal.sibships.PATH<-"Other")

		#Caveat for if the delimiter is OTHER
		while(length(colonyfile$delim.for.excluded.paternal.sibships.PATH)=="Other"){
		if(colonyfile$delim.for.excluded.paternal.sibships.PATH=="Other"){
		cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
		colonyfile$delim.for.excluded.paternal.sibships.PATH<-scan(n=1,what="character")}}

#Read in the data...
colonyfile$excluded.paternal.sibships<-read.table(colonyfile$excluded.paternal.sibships.PATH,header=FALSE,sep=colonyfile$delim.for.excluded.paternal.sibships.PATH,colClasses=c("character"))

#Check the data
if(colonyfile$n.excluded.paternal.sibships!=dim(colonyfile$excluded.paternal.sibships)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="excluded.paternal.sibships.PATH")];
flush.console();
warning(paste("The number of defined excluded paternal sibships ","(", colonyfile$n.excluded.paternal.sibships,") does not equal the number provided in the file selected (", dim(colonyfile$excluded.paternal.sibships)[1],").\n\n",sep=""),immediate.=TRUE)
}}



#ORJ: The formatting for this needs to be checked with Jinliang.
colonyfile$excluded.paternal.sibships[,1+dim(colonyfile$excluded.paternal.sibships)[2]]<-c("!Size of known excluded paternal sibship, and IDs of excluded offspring in the sibship",rep("",dim(colonyfile$excluded.paternal.sibships)[1]-1))
csum<-NULL
for (i in 1:dim(colonyfile$excluded.paternal.sibships)[1]){
csum[i]<-length(colonyfile$excluded.paternal.sibships[i,][!is.na(colonyfile$excluded.paternal.sibships[i,])])}
rownames(colonyfile$excluded.paternal.sibships)<-csum

write.table(paste(colonyfile$n.excluded.paternal.sibships,"!Number of offspring with known excluded paternal sibships"),name,append=TRUE,quote=FALSE,row.names=TRUE,col.names=FALSE)

write.table(colonyfile$known.maternities,name,append=TRUE,quote=FALSE,na=" ",row.names=TRUE,col.names=FALSE)
write("",name,append=TRUE)

}else{
#If there are no excluded maternities
write.table(paste(colonyfile$n.excluded.paternal.sibships," !Number of offspring with known excluded paternal sibships"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)
}




#######################################################
#Define EXCLUDED MATERNAL sibships
#######################################################

cat("Enter the number of offspring with known excluded maternal sibships.\n\n\n")
colonyfile$n.excluded.maternal.sibships<-scan(n=1,what="integer")

if(colonyfile$n.excluded.maternal.sibships>0){


#Get the path, and delimiter, to the file...
while(length(colonyfile$excluded.maternal.sibships.PATH)==0){
	cat("Provide the path to the excluded MATERNAL sibships file.\n\n\n")
	flush.console()
	colonyfile$excluded.maternal.sibships.PATH<-file.choose()

	cat("What is the delimiter for this file?\n\n\n")
	flush.console()
	switch(menu(c("Whitespace", "Tab","Comma", "Other")) + 1,cat("Nothing done\n\n\n"), colonyfile$delim.for.excluded.maternal.sibships.PATH<-"", colonyfile$delim.for.excluded.maternal.sibships.PATH<-"\t", colonyfile$delim.for.excluded.maternal.sibships.PATH<-",",delim.for.excluded.maternal.sibships.PATH<-"Other")

		#Caveat for if the delimiter is OTHER
		while(length(colonyfile$delim.for.excluded.maternal.sibships.PATH)=="Other"){
		if(colonyfile$delim.for.excluded.maternal.sibships.PATH=="Other"){
		cat("You chose OTHER. Please enter the delimiter for this file.\n\n\n")
		colonyfile$delim.for.excluded.maternal.sibships.PATH<-scan(n=1,what="character")}}

#Read in the data...
colonyfile$excluded.maternal.sibships<-read.table(colonyfile$excluded.maternal.sibships.PATH,header=FALSE,sep=colonyfile$delim.for.excluded.maternal.sibships.PATH,colClasses=c("character"))

#Check the data
if(colonyfile$n.excluded.maternal.sibships!=dim(colonyfile$excluded.maternal.sibships)[1]){
colonyfile<-colonyfile[which(names(colonyfile)!="excluded.maternal.sibships.PATH")];
flush.console();
warning(paste("The number of defined excluded maternal sibships ","(", colonyfile$n.excluded.maternal.sibships,") does not equal the number provided in the file selected (", dim(colonyfile$excluded.maternal.sibships)[1],").\n\n",sep=""),immediate.=TRUE)
}}



#ORJ: The formatting for this needs to be checked with Jinliang.
colonyfile$excluded.maternal.sibships[,1+dim(colonyfile$excluded.maternal.sibships)[2]]<-c("!Size of known excluded maternal sibship, and IDs of excluded offspring in the sibship",rep("",dim(colonyfile$excluded.maternal.sibships)[1]-1))
csum<-NULL
for (i in 1:dim(colonyfile$excluded.maternal.sibships)[1]){
csum[i]<-length(colonyfile$excluded.maternal.sibships[i,][!is.na(colonyfile$excluded.maternal.sibships[i,])])}
rownames(colonyfile$excluded.maternal.sibships)<-csum

write.table(paste(colonyfile$n.excluded.maternal.sibships,"!Number of offspring with known excluded maternal sibships"),name,append=TRUE,quote=FALSE,row.names=TRUE,col.names=FALSE)

write.table(colonyfile$known.maternities,name,append=TRUE,quote=FALSE,na=" ",row.names=TRUE,col.names=FALSE)
write("",name,append=TRUE)

}else{
#If there are no excluded maternities
write.table(paste(colonyfile$n.excluded.maternal.sibships," !Number of offspring with known excluded maternal sibships"),name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
write("",name,append=TRUE)
}





cat("Finished!")
cat(paste("Your file is called",name,"and is placed in",wd,"...\n\n\n"))

return(colonyfile)

}    