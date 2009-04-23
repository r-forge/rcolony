build.colony.input<-function(wd=getwd(),name="Colony2.DAT"){

colonyfile<-NULL

cat("This function will construct a Colony input file.\n")
cat(paste("It will be called",name,"and be placed in",wd,"...\n\n\n"))

#  ! C, Dataset name, Length<51
while(length(colonyfile$datasetname)==0){
cat("Enter dataset name (must be <51 characters).\n")
colonyfile$datasetname<-scan(n=1,what="character")
write(paste(colonyfile$datasetname,"! C, Dataset name, Length<51"),name,append=FALSE)}

#  ! C, Main output file name, Length<21
while(length(colonyfile$outfile)==0){
cat("Enter main output file name (must be <21 characters).\n")
colonyfile$outfile<-scan(n=1,what="character")
write(paste(colonyfile$outfile,"! C, Main output file name, Length<21"),name,append=TRUE)}

#  ! I, Number of offspring in the sample
while(length(colonyfile$n.offspring)==0){
cat("Enter number of offspring in the sample.\n")
colonyfile$n.offspring<-scan(n=1,what="integer")
write(paste(colonyfile$n.offspring,"! I, Number of offspring in the sample"),name,append=TRUE)}

#  ! I, Number of loci
while(length(colonyfile$n.loci)==0){
cat("Enter number of loci.\n")
colonyfile$n.loci<-scan(n=1,what="integer")
write(paste(colonyfile$n.loci,"! I, Number of loci"),name,append=TRUE)}

#  ! I, Seed for random number generator
while(length(colonyfile$rseed)==0){
cat("Enter seed for random number generator.\n")
colonyfile$rseed<-scan(n=1,what="integer")
write(paste(colonyfile$rseed,"! I, Seed for random number generator"),name,append=TRUE)}

#  ! B, 0/1=Not updating/updating allele frequency
cat("Should allele frequency be updated?\n")
switch(menu(c("Not updating allele frequency", "Updating allele frequency")) + 1,
       cat("Nothing done\n"), colonyfile$updateallelefreq<-0, colonyfile$updateallelefreq<-1)
write(paste(colonyfile$updateallelefreq,"! B, 0/1=Not updating/updating allele frequency"),name,append=TRUE)

#  ! B, 0/1=Diploid species/HaploDiploid species
cat("What kind of species is it?\n")
switch(menu(c("Diploid species", "HaploDiploid species")) + 1,
       cat("Nothing done\n"), colonyfile$speciestype<-0, colonyfile$speciestype<-1)
write(paste(colonyfile$speciestype,"! B, 0/1=Diploid species/HaploDiploid species"),name,append=TRUE)

#  ! B, 0/1=Polygamy/Monogamy for males & females
cat("Are males monogamous or polygamous?\n")
switch(menu(c("Males monogamous", "Males polygamous")) + 1,
       cat("Nothing done\n"), colonyfile$malepolygamy<-0, colonyfile$malepolygamy<-1)

cat("Are females monogamous or polygamous?\n")
switch(menu(c("Females monogamous", "Females polygamous")) + 1,
       cat("Nothing done\n"), colonyfile$femalepolygamy<-0, colonyfile$femalepolygamy<-1)
write(paste(colonyfile$malepolygamy,colonyfile$femalepolygamy,"! B, 0/1=Polygamy/Monogamy for males & females"),name,append=TRUE)

#  ! B,R,R : Use sibship prior, Y/N=1/0. If Yes, give mean paternal, maternal sibship size
cat("Use sibship prior?\n")
switch(menu(c("YES", "NO")) + 1,
       cat("Nothing done\n"), colonyfile$sibship.prior<-1, colonyfile$sibship.prior<-0)

if(colonyfile$sibship.prior==0){
	colonyfile$sibship.prior.paternal<-0
	colonyfile$sibship.prior.maternal<-0
write(paste(colonyfile$sibship.prior,colonyfile$sibship.prior.paternal,colonyfile$sibship.prior.maternal,"! B,R,R : Use sibship prior, Y/N=1/0. If Yes, give mean paternal, maternal sibship size"),name,append=TRUE)
	}else{

while(length(colonyfile$sibship.prior.paternal)==0){
cat("Enter the paternal sibship size.\n")
colonyfile$sibship.prior.paternal<-scan(n=1,what="integer")}

while(length(colonyfile$sibship.prior.maternal)==0){
cat("Enter the maternal sibship size.\n")
colonyfile$sibship.prior.maternal<-scan(n=1,what="integer")}

write(paste(colonyfile$sibship.prior,colonyfile$sibship.prior.paternal,colonyfile$sibship.prior.maternal,"! B,R,R : Use sibship prior, Y/N=1/0. If Yes, give mean paternal, maternal sibship size"),name,append=TRUE)
}

#  ! B, 0/1=Unknown/Known population allele frequency
cat("Unknown/Known population allele frequency?\n")
switch(menu(c("Unknown", "Known")) + 1,
       cat("Nothing done\n"), colonyfile$knownAFreq<-0, colonyfile$knownAFreq<-1)
write(paste(colonyfile$knownAFreq,"! B, 0/1=Unknown/Known population allele frequency"),name,append=TRUE)

#  ! I, Number of runs
while(length(colonyfile$n.runs)==0){
cat("Number of runs.\n")
colonyfile$n.runs<-scan(n=1,what="integer")
write(paste(colonyfile$n.runs,"! I, Number of runs"),name,append=TRUE)}

#  ! I, Length of Run (1, 2, 3) = (Short, Medium, Long)
while(length(colonyfile$runlength)==0){
cat("Length of run?\n")
switch(menu(c("Short", "Medium","Long")) + 1,
       cat("Nothing done\n"), colonyfile$runlength<-1, colonyfile$runlength<-2, colonyfile$runlength<-3)
write(paste(colonyfile$runlength,"! I, Length of Run (1, 2, 3) = (Short, Medium, Long)"),name,append=TRUE)}


#  ! B, 0/1=Monitor method by Iterate#/Time in second
cat("Monitor method by Iterate/Time in second?\n")
switch(menu(c("Monitor by iterate", "Monitor by time in seconds")) + 1,
       cat("Nothing done\n"), colonyfile$monitortype<-0, colonyfile$monitortype<-1)
write(paste(colonyfile$monitortype,"! B, 0/1=Monitor method by Iterate#/Time in second"),name,append=TRUE)

#  ! I, Monitor interval in Iterate#/Seconds
while(length(colonyfile$interval)==0){
cat("Monitor interval (in iterate number or seconds) depending on how you have chosen to monitor progress.\n")
colonyfile$interval<-scan(n=1,what="integer")
write(paste(colonyfile$interval,"! I, Monitor interval in Iterate#/Seconds"),name,append=TRUE)}

#  ! B, 0/1=Other platform/Windows execution
cat("What platform is this to be executed on?\n")
switch(menu(c("Microsoft Windows system", "Other system (e.g. Mac/Unix)")) + 1,
       cat("Nothing done\n"), colonyfile$sys<-1, colonyfile$sys<-0)
write(paste(colonyfile$sys,"! B, 0/1=Other platform/Windows execution"),name,append=TRUE)

#  ! 1/0=Full-likelihood/pair-likelihood score method
cat("Which likelihood method should be used?\n")
switch(menu(c("Full likelihood", "Pairwise likelihood")) + 1,
       cat("Nothing done\n"), colonyfile$likelihood.method<-0, colonyfile$likelihood.method<-1)
write(paste(colonyfile$likelihood.method,"! 1/0=Full-likelihood/pair-likelihood score method"),name,append=TRUE)

#  ! 1/2/3=low/medium/high precision
cat("What level of precision should be used?\n")
switch(menu(c("Low", "Medium","High")) + 1,
       cat("Nothing done\n"), colonyfile$precision<-1, colonyfile$precision<-2, colonyfile$precision<-3)
write(paste(colonyfile$precision,"! 1/2/3=low/medium/high precision"),name,append=TRUE)

#Give the path to the marker types and error rate file. This should be a file with a number of columns equal to the number of markers used.
#There should be 4 rows, 1) marker ID, 2) marker type, 3) marker specific allelic dropout rate, 4) marker specific other typing error rate.

while(length(colonyfile$MarkerPATH)==0){
cat("Provide the path to the Marker Types and Error Rate file.\n");Sys.sleep(1)
flush.console()
colonyfile$MarkerPATH<-file.choose()}

colonyfile$Markers<-read.table(colonyfile$MarkerPATH,header=TRUE,colClasses=c("character")) 
colonyfile$Markers[,1+dim(colonyfile$Markers)[2]]<-c("!Marker IDs","!Marker types","!Marker specific allelic dropout rate","!Marker specific other typing-error rate")
write.table(colonyfile$Markers,name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)

#Give the path to the offpring ID and genotype file
#This should have a first column giving the ID, then 2 columns for each locus (1 for each allele at that locus).
#Therefore, with 4 loci, there should be 9 columns.

cat("Provide the path to the offspringID and genotype file.\n")
flush.console()
while(length(colonyfile$OSGenotypePATH)==0){
colonyfile$OSGenotypePATH<-file.choose()}

colonyfile$Offspring<-read.table(colonyfile$OSGenotypePATH,header=TRUE,colClasses=c("character")) 
colonyfile$Offspring[,1+dim(colonyfile$Offspring)[2]]<-c("!Offspring ID and genotypes",rep("",dim(colonyfile$Offspring)[1]-1))
write.table(colonyfile$Offspring,name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)

#Sampling
while(length(colonyfile$dadprob)==0){
cat("What is the probability that the FATHER of an offpring is included in the candidate set?\n E.g. 0.5\n")
colonyfile$dadprop<-scan(n=1,what="integer")}

while(length(colonyfile$mumprob)==0){
cat("What is the probability that the MOTHER of an offpring is included in the candidate set?\n E.g. 0.5\n")
colonyfile$mumprob<-scan(n=1,what="integer")}

write(paste(colonyfile$dadprob,colonyfile$mumprob,"!Prob that the dad and mum of an offspring included in candidates"),name,append=TRUE)

#Number of candidate mothers and fathers
while(length(colonyfile$n.dad)==0){
cat("How many candidate MOTHERS are there?\n")
colonyfile$n.dad<-scan(n=1,what="integer")}

while(length(colonyfile$n.mum)==0){
cat("How many candidate MOTHERS are there?\n")
colonyfile$n.mum<-scan(n=1,what="integer")}

write(paste(colonyfile$n.dad,colonyfile$n.mum,"!Numbers of candidate males and females"),name,append=TRUE)

#Candidate FATHERS
cat("Provide the path to the candidate FATHERS file.\n")
flush.console()
while(length(colonyfile$dadsPATH)==0){
colonyfile$dadsPATH<-file.choose()}

colonyfile$dads<-read.table(colonyfile$dadsPATH,header=TRUE,colClasses=c("character")) 
colonyfile$dads[,1+dim(colonyfile$dads)[2]]<-c("!Candidate M ID and genotypes",rep("",dim(colonyfile$dads)[1]-1))
write.table(colonyfile$dads,name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)


#Candidate MOTHERS
cat("Provide the path to the candidate MOTHERS file.\n")
flush.console()
while(length(colonyfile$mumsPATH)==0){
colonyfile$mumsPATH<-file.choose()}

colonyfile$mums<-read.table(colonyfile$mumsPATH,header=TRUE,colClasses=c("character")) 
colonyfile$mums[,1+dim(colonyfile$mums)[2]]<-c("!Candidate F ID and genotypes",rep("",dim(colonyfile$mums)[1]-1))
write.table(colonyfile$mums,name,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)




 
#     0                       !Number of offspring with known father
 
#     0                       !Number of offspring with known mother
 
 
#     0                       !Number of known paternal sibships
 
#     0                       !Number of known maternal sibships
 
#     0                       !Number of offspring with known excluded candidates as father
 
#     0                       !Number of offspring with known excluded candidates as mother
 
#     0                       !Number of offspring with known excluded paternal sibships
 
#     0                       !Number of offspring with known excluded maternal sibships




cat("Finished!")
cat(paste("Your file is called",name,"and is placed in",wd,"...\n\n\n"))




}    
