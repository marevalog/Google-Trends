library(testthat)

# import the files in outTrendsFolder as relevant lists
ImportOutTrends <- function(outTrendsFolder,patternFiles="google_report.csv$"){
  # get the files to import
  files <- list.files(path=outTrendsFolder,pattern=patternFiles,full.names=TRUE)

  # import all files
  occurences <- list()
  geo <- list()
  period <- list()
  request <- list()
  for(iRequest in 1:length(files)){
    fileIn <- files[iRequest]

    rawOut <- scan(file=fileIn,what="character",sep="\n",blank.lines.skip=FALSE)
    request[[iRequest]] <- gsub(".*: ","",rawOut[1])
    geoTime <- strsplit(rawOut[2],";")[[1]]
    geo[[iRequest]] <- geoTime[1]
    period[[iRequest]] <- geoTime[2]

    blocksLines <- c(1,which(rawOut == ""))
    if(length(blocksLines)<3){
      warning(paste("something wrong for",fileIn,"no outputs (likely not enough volume of searches)"))
      next()
    }

    cat("Treating",rawOut[1],"\n")
    blockOccurence <- rawOut[(blocksLines[2]+1):(blocksLines[3]-1)]
    expect_true(grep("Interest over time",blockOccurence)==1)
    nameCols <- strsplit(blockOccurence[2],",")[[1]]
    occurences[[iRequest]] <- as.data.frame(t(simplify2array(strsplit(blockOccurence[-(1:2)],","))))
    names(occurences[[iRequest]]) <- nameCols
  }
  return(list(geo=geo,date=period,search=request,oc=occurences))
}

# format the times to standard mode
TimeFromOutTrends <- function(dat,datCol=1){
  nameDatCol <- names(dat)[datCol]
  # cat("nameDatCol:",nameDatCol,"\n")
  if(nameDatCol=="Month"){
    beginTimes<-as.Date(paste0(as.character(dat[,datCol]),"-01"),format="%Y-%m-%d")
  }else if(nameDatCol == "Week"){
    beginTimes<-as.Date(simplify2array(strsplit(as.character(dat[,datCol])," "))[1,],
			format="%Y-%m-%d")
  }
  return(beginTimes)
}

# if the time step is the week, change it to the month and format dates
GetDatByMonth<-function(dat,datCol=1){
  nameDatCol <- names(dat)[datCol]
  # make sure everything is in right format
  times <- TimeFromOutTrends(dat)
  datByMonth <- dat
  dat[,1] <- times
  for(iCol in 2:dim(dat)[2]){
    dat[,iCol]<- as.numeric(as.character(dat[,iCol]))
  }

  if(nameDatCol=="Month"){
    datByMonth <- dat
  }else if(nameDatCol=="Week"){
    # initialize the table
    datByMonth <- dat[1,]
    names(datByMonth)[1] <- "Month"

    # fill it month by month
    require("lubridate")
    initTime <- min(times)
    endTime <- max(times)

    #determine where month begins
    initMonth <-initTime
    day(initMonth) <- 1
    
    iTime <- 1
    while(initMonth <endTime){
      initNextMonth <- initMonth
      month(initNextMonth) <- month(initMonth)+1
      toSum <- which(times>= initMonth & times < initNextMonth)
      datByMonth[iTime,1] <- initMonth
      sumReq <- apply(as.matrix(as.numeric(as.character(dat[toSum,-1]))),2,sum,na.rm=TRUE)
      # if(is.na(sumReq)){
      #   browser()
      # }else{
      #   cat("iTime:",iTime,"sumReq:",sumReq,"\n")
      # }
      datByMonth[iTime,-1] <- sumReq

      initMonth <- initNextMonth
      iTime <- iTime+1
    }
  }
  return(datByMonth)
}

# create new devices (not in the Rstudio device)
devNew <- function(...){
  OS <- Sys.info()['sysname']
  if(OS == "Linux"){
    X11(...)
  }else if(OS == "Windows"){
    windows()
  }else if(OS == "Mac"){
    quartz()
  }
}

