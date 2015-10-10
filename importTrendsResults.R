graphics.off()
#install.packages("lubridate") 


# TODO: bed bugs seems to be treated as two words should not be
# Extract from pseudo csv files from Trends the occurences
# CMBC
# GPL last version

# using "bed bugs" as ref
#outTrendsFolder <- "requestsResults_2014-01-23_16-16-14" 

# using bedbugs as ref
# outTrendsFolder <- "requestsResults_2014-01-23_16-59-24" # where to put the results from google trends

# # using "bed bugs exterminator" as ref
# outTrendsFolder <- "requestsResults_2014-01-23_19-09-39" # where to put the results from google trends
#=> sometimes even relevant things litterally disappear (not given in csv by google)

# # invalid for some reason (eroneous pasword ?)
# outTrendsFolder <- "requestsResults_2014-01-24_18-10-52" # where to put the results from google trends

# using "bedbugs exterminator" as ref
# outTrendsFolder <- "requestsResults_2014-01-24_18-19-59" # where to put the results from google trends
#=> sometimes bedbugs exterminator disappear
# what seems to happen is if something is too small, something will disappear, not necessary the too small things!!!
#=> should really search for homogeneous terms in terms of frequency, which is difficult to know before
#   it happens
#   What is sure is: 
#    - having three searches at the same time is increasing the probability of troubles
#    - bedbugs is much less frequent
#   So we should probably 
#    - run everything separatly first, this will
#          - tell us about the regularity
#          - the spikiness, even allow will tell something about the volume of searches

# # using only "bed bugs" as key word and without references
# outTrendsFolder <- "requestsResults_2014-01-24_18-39-11"
#=> ok, Nota, up to here only US

# # All world 
# outTrendsFolder <- "requestsResults_2014-01-24_20-32-19" 

# All world, improved terms
#outTrendsFolder <- "requestsResults_2014-02-03_18-03-51" 

# All world, improved terms second try
#outTrendsFolder <- "requestsResults_2014-02-18_23-25-53/"

# All world, pest comparison
#outTrendsFolder <- "requestsResults_2014-02-10_17-30-40"

# All world, last round selection terms
#outTrendsFolder <- "requestsResults_2014-03-06_13-05-43/" 

# All world, terms with volume of searches selection terms
#outTrendsFolder <- "requestsResults_2014-03-17_16-38-09/"

# united states, 2004-present compare bed bugs exterminator
#outTrendsFolder <- "requestsResults_2014-04-18_15-29-36/"

# CHANGE NAME OF requestsResults folder. VARIES FOR EVERY RUN OF .PY FILE
# united states, 2004-present no comparison
outTrendsFolder <- "requestsResults_2014-08-29_13-25-26"

# united states, bed bugs april 2004-sept 11 2014 no comparison
#outTrendsFolder <- "requestsResults_2014-09-11_08-28-54_bed_bugs"

# united states, bedbugs april 2004-sept 11 2014 no comparison
#outTrendsFolder <- "requestsResults_2014-09-11_08-23-02_bedbugs"

# set the term of reference here
reference <- "bed bugs exterminator"
#reference <- "bedbugs exterminator"

outFile <- paste0(outTrendsFolder,"/sumTable.Rda")

# functions 
source("importTrendsResultsFns.R")

dat<-ImportOutTrends(outTrendsFolder)
occurences <- dat$oc

# check that all requests had same time, same reference
# for some reason, if very low occurences, google may just ignore some requests
# it is the same with the interface
usingRef <- (dim(occurences[[1]])[2]>2 && 
	     names(occurences[[1]])[2] == names(occurences[[2]])[2] )
if(usingRef){ # if using a reference
  for(iRequest in 1:length(occurences)){
    expect_equal(occurences[[iRequest]][,1],occurences[[1]][,1])
  }
}
if(!usingRef){ # if not using a reference plot directly
  devNew()
  par(mfrow=c(3,3))
  sumTable <- NULL
  for(iRequest in 1:length(occurences)){
    dat <- occurences[[iRequest]]
    dat[,2] <- as.numeric(as.character(dat[,2]))
    if(!is.null(dat)){
      plot(TimeFromOutTrends(dat),dat[,2],type="l",main=paste(iRequest,":",names(dat)[2]))
      # make a general database using the Month for everybody
      datByMonth <- GetDatByMonth(dat)
      if(is.null(sumTable)){
	sumTable <- datByMonth
      }else{
	expect_equal(datByMonth[,1],sumTable[,1])
	sumTable<-cbind(sumTable,datByMonth[,-1])
	nCol <- dim(sumTable)[2]
	newColumns <- (nCol-(dim(datByMonth)[2]-1)+1):nCol
	names(sumTable)[newColumns] <- names(datByMonth)[-1]
      }
    }else{
      plot(1)
    }
  }
  # plot only the OK ones
}

### make one table for all the requests
if(usingRef){
  # add the initial reference
  sumTable <- occurences[[2]][,1:2] # using [[2]] as the first is splashed by "bed bugs"
  sumTable[,2] <- as.numeric(as.character(sumTable[,2]))

  # add all terms
  for(iRequest in 1:length(occurences)){
    # isolate the occurences
    oc <- occurences[[iRequest]][,-(1:2)]
    # make a name for occurences
    namesAll <- names(occurences[[iRequest]])
    items <- gsub(paste0(reference," "),"",namesAll[length(namesAll)])
    # items <- strsplit(namesAll[length(namesAll)]," ")[[1]]
    nameCol <- items[length(items)]

    # summarize the occurences if needed
    if(class(oc)!="data.frame"){
      # convert to numeric
      oc <- as.numeric(as.character(oc))
    }else{
      # convert to numeric
      for(iCol in 1:length(oc)){
	oc[,iCol] <- as.numeric(as.character(oc[,iCol]))
      }
      # add the two columns
      oc <- apply(oc,1,sum)
      # standardize by bedbugs
    }
    # normalize by maximum of ref term
    ref <- as.numeric(as.character(occurences[[iRequest]][,2]))
    maxRef <- max(ref,na.rm=TRUE)
    oc <- oc/maxRef

    # summarize occurences
    sumTable <- cbind(sumTable,oc)
    colnames(sumTable)[length(names(sumTable))]<-nameCol
  }

  beginTimes <- TimeFromOutTrends(sumTable)
  sumTable <- cbind(beginTimes,sumTable)


  # simple plot
  
  plotSumTable <- sumTable[,-(1:3)]
  termsSearch <- names(plotSumTable)
  maxTerms<-max(plotSumTable[,termsSearch],na.rm=TRUE)
  devNew()
  plot(c(min(beginTimes),max(beginTimes)),c(0,maxTerms),type="n",xlab="Time (Years)",ylab="Search frequency")
  for(iTerm in 1:length(termsSearch)){
    lines(beginTimes,plotSumTable[,termsSearch[iTerm]],col=iTerm,type="l",lty=3)
  }
  lines(beginTimes,plotSumTable[,reference],lty=1,col="blue") 
  # => almost impossible to see anything due to parameters
  #    needs to find how to identify

  # plot them all
  for(iTerm in 1:length(termsSearch)){
    if((iTerm-1) %% (3*4)==0){
      devNew()
      par(mfrow=c(3,4))
    }
    plot(beginTimes,plotSumTable[,termsSearch[iTerm]],type="l",ylab=termsSearch[iTerm])
  }
}
# save the plot
save(sumTable,file=outFile)


# Make "zeros" analysis

countZero <- function(vect){
  return(length(which(as.numeric(vect)==0)))
}
nMaxZero <- 0
beginCountZero <- min(which((sumTable$beginTimes>=as.Date("2011-04-01"))==TRUE))
endCountZero <- max(which((sumTable$beginTimes<as.Date("2014-04-01"))==TRUE))

#i moved up to september MIKE
#endCountZero <- max(which((sumTable$beginTimes<as.Date("2014-09-01"))==TRUE))
+
toTrashColumns <- which(apply(sumTable[beginCountZero:endCountZero,-(1:3)],
                              2,countZero)>nMaxZero)
# Make total hits analysis
nTopKept <- 25
totSearches <- apply(sumTable[beginCountZero:endCountZero,-(1:3)],2,sum,na.rm=TRUE)
toKeepTopSearches <- sort(totSearches,decreasing=TRUE)[1:nTopKept]
intersect(names(toTrashColumns),names(toKeepTopSearches))

# bilan
bilan <- cbind(totSearches,1)
bilan[toTrashColumns,2]<-0
bilan <- bilan[order(totSearches,decreasing=TRUE),]
bilan <- cbind(1:dim(bilan)[1],bilan)

