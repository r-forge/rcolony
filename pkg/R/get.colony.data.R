get.colony.data<-function(datadir, filename = list.files(datadir, pattern = ".DAT", ignore.case=TRUE)){
    
    #Test to see if colony has finished running.
    t1 <- list.files(path = datadir, pattern = "Maternity")[1]
    
    if(is.na(t1[1])){
    	stop("The required COLONY output files aren\'t there.\nCheck that colony has finished running.")
    	}

    #Test consistency of the database file. Throw warnings/errors as necessary
    colony.object <- NULL
    x <- readLines(paste(datadir, filename, sep = "/"))
    
    #Strip out empty rows, if there are any.
    if(length(which(x == "")) > 0){x = x[-which(x == "")]}

    #Extract the number of offspring from the dat file. This information is used for error checking later on.
    n <- x[3]
    n <- sub("^[\t\n\f\r ]*", "", n) #remove leading whitespace
    n <- as.numeric(gsub("([A-Za-z0-9]*)([!0-9A-Za-z ]*)", "\\1", n, perl = TRUE))
    
    ###################################################
    #Lists of offspring, fathers and mothers
    ###################################################
    offspring <- x[grep("!Offspring ID and genotypes",x):(grep("!Prob that the dad and mum of an offspring included in candidates", x) - 2)] #M for male, F for female
    offspring <- offspring[offspring != ""]
    
    fathers <- x[grep("!Candidate M ID and genotypes",x):(grep("!Candidate F ID and genotypes",x) - 2)] #M for male, F for female
    fathers <- fathers[fathers != ""]
    
    mothers <- x[grep("!Candidate F ID and genotypes",x):(grep("!Number of offspring with known father",x) - 3)] #M for male, F for female
    mothers <- mothers[mothers != ""]
    
    #remove leading whitespace
    offspring <- sub("^[\t\n\f\r ]*", "", offspring) #remove leading whitespace
    fathers <- sub("^[\t\n\f\r ]*", "", fathers) #remove leading whitespace
    mothers <- sub("^[\t\n\f\r ]*", "", mothers) #remove leading whitespace
    
    offspring <- as.vector(sapply(offspring, function(x){gsub("([A-Za-z0-9]*)([!0-9A-Za-z ]*)", "\\1", x, perl = TRUE)})) #extract names
    fathers <- as.vector(sapply(fathers, function(x){gsub("([A-Za-z0-9]*)([!0-9A-Za-z ]*)", "\\1", x, perl = TRUE)})) #extract names
    mothers <- as.vector(sapply(mothers, function(x){gsub("([A-Za-z0-9]*)([!0-9A-Za-z ]*)", "\\1", x, perl = TRUE)})) #extract names
    
    if(n != length(offspring)){stop("Wrong number of offspring. Check your files.")}else{}
    
    #Assign numeric codes to fathers and mothers
    mothers <- data.frame(motherID=mothers, mother.numID = as.numeric(as.factor(mothers)))
    mothers$motherID <- as.character(mothers$motherID)
    mothers <- rbind(mothers, c("Unknown", max(mothers$mother.numID) + 1))
    
    fathers <- data.frame(fatherID=fathers, father.numID = as.numeric(as.factor(fathers)))
    fathers$fatherID <- as.character(fathers$fatherID)
    fathers <- rbind(fathers, c("Unknown",max(fathers$father.numID) + 1))
    		   
    offspring <- data.frame(offspringID=offspring, offspring.numID = as.numeric(as.factor(offspring)))
    offspring$offspringID <- as.character(offspring$offspringID)
    offspring <- rbind(offspring, c("Unknown", max(offspring$offspring.numID) + 1))
    
    colony.object$fathers <- fathers
    colony.object$mothers <- mothers
    colony.object$offspring <- offspring
    
    ###################################################
    #assigned parentage (nonpairwise)
    ###################################################
    mfile <- list.files(path = datadir, pattern = ".Maternity")[1]
    if(length(mfile) > 2){
    	warning("There are too many \"\ *.Maternity\" files in your project directory. \nYou should check them.")
    }
    
    maternity <- read.table(paste(datadir, mfile, sep=""), header = TRUE)
    
    pfile <- list.files(path = datadir, pattern = ".Paternity")[2]
    if(length(pfile) > 2){
    	warning("There are too many \"\ *.Paternity\" files in your project directory. \nYou should check them.")
    }
    paternity <- read.table(paste(datadir, pfile, sep=""), header = TRUE)
    
    colony.object$maternity <- maternity
    colony.object$paternity <- paternity
    
    ###################################################
    #assigned parentage (pairwise)
    ###################################################
    mfile <- list.files(path = datadir, pattern = ".PairwiseMaternity")[1]
    if(length(mfile) > 1){
    	warning("There are too many \"\ *.PairwiseMaternity\" files in your project directory. \nYou should check them. \nUsing the first one.")
    }
    pairwise.maternity <- read.table(paste(datadir, mfile, sep = ""), header = TRUE)
    
    pfile <- list.files(path = datadir, pattern = ".PairwisePaternity")[1]
    if(length(mfile) > 1){
    	warning("There are too many \"\ *.PairwisePaternity\" files in your project directory. \nYou should check them. \nUsing the first one.")
    }
    pairwise.paternity <- read.table(paste(datadir, pfile, sep=""), header = TRUE)
    
    colony.object$pairwise.maternity <- pairwise.maternity
    colony.object$pairwise.paternity <- pairwise.paternity
    
    
    ###################################################
    #Sibships (nonpairwise)
    ###################################################
    #Get full and half sibship data.
    full.file <- list.files(path = datadir, pattern = ".FullSibDyad")[1]
    half.file <- list.files(path = datadir, pattern = ".HalfSibDyad")[1]
    
    fullsibs <- read.table(paste(datadir, full.file, sep = ""), header = TRUE)
    halfsibs <- read.table(paste(datadir, half.file, sep = ""), header = TRUE)
    fullsibs$type <- rep("Full", dim(fullsibs)[1])
    halfsibs$type <- rep("Half", dim(halfsibs)[1])
    sibs <- rbind(fullsibs, halfsibs)
    sibs$type <- as.factor(sibs$type)
    
    colony.object$sibs <- sibs
    
    ###################################################
    #Sibships (pairwise)
    ###################################################
    
    #Get full and half sibship data.
    full.file <- list.files(path = datadir, pattern = ".PairwiseFullSibDyad")[1]
    half.file <- list.files(path = datadir, pattern = ".PairwiseHalfSibDyad")[1]
    
    fullsibs <- read.table(paste(datadir, full.file, sep = ""), header = TRUE)
    halfsibs <- read.table(paste(datadir, half.file, sep = ""), header = TRUE)
    fullsibs$type <- rep("Full", dim(fullsibs)[1])
    halfsibs$type <- rep("Half", dim(halfsibs)[1])
    sibs <- rbind(fullsibs, halfsibs)
    sibs$type <- as.factor(sibs$type)
    
    colony.object$pairwise.sibs <- sibs
    
    return(colony.object)
    }

