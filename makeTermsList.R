#setwd("~/Dropbox/Bed_bug_searches_project/AutomatedRetrieval")
setwd("~/GitHub/Google-Trends")

#!/usr/bin/Rscript
# Makes from termsList.txt
# a list of requests that can be handled by termsFromTxt.py
# CMBC
# GNU current version

# retrieve the terms
#initial list
#secondaryTerms <- read.csv("termsList.txt",header=FALSE,as.is=TRUE)[,1]
#extended list (Autocomplete+Correlate)
#secondaryTerms <- read.csv("termsList2.txt",header=FALSE,as.is=TRUE)[,1]

secondaryTerms <- read.csv("termsList.txt",header=FALSE,as.is=TRUE)[,1]

#pest comparison
#secondaryTerms <- read.csv("termsList3.txt",header=FALSE,as.is=TRUE)[,1]

# folder for requests ! This should only contain the request files
requestsFolder <- "requestsListings"

### check if requests folder exists, or create it
unlink(requestsFolder,recursive=TRUE,force=TRUE)
if( ! file.exists(requestsFolder)){
  dir.create(requestsFolder)
}

# main keywords the requests should be combined with
mainKeyWords <- c("bed bugs") # "" means no keyword
#referenceSearch <- "bed bugs exterminator" # "" means no reference search
referenceSearch <- "" # "" means no reference search


nRequest = length(secondaryTerms)

# make all the requests
requests <- list()
for(iRequest in 1:nRequest){
  if(referenceSearch != ""){
    requests[[iRequest]] <- c(referenceSearch,paste(mainKeyWords, secondaryTerms[iRequest]))
  }else{
    requests[[iRequest]] <- paste(mainKeyWords, secondaryTerms[iRequest])
  }
}

# save requests as different files
for(iRequest in 1:nRequest){
  fileName <- paste0(requestsFolder,"/request",secondaryTerms[iRequest],".txt")
  write.table(file=fileName,requests[[iRequest]],row.names=FALSE,col.names=FALSE,quote=FALSE)
}

