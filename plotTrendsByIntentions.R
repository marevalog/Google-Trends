# use simple fake survey classifying all the terms by Daniel
# in Prevention, Identification, Control (1,2,3 and 0 for none or hard ties)
graphics.off()

nRepet <- 1000 # the number of repetitions in bootstrap

#### import the class corresponding to each term
classes <- read.csv("extendedtermscategory.csv")
mainTerms <- c("bed bugs")
intentions <- c("?","Don't have","Might","Have")

#### load the trends per term
#### this name may change according to your naming convention
#### this follows our current naming convention
load("requestResults/sumTable.Rda")

termsFound <- colnames(sumTable)
for(term in mainTerms){
  termsFound <- gsub(paste0("^",mainTerms),"",termsFound)
  termsFound <- gsub("^ ","",termsFound) # removing initial " "
  termsFound <- gsub(" $","",termsFound) # removing trailing " "
}

#### check the terms match
#### transform everything to lower case
colnames(sumTable) <- tolower(colnames(sumTable))
classes$term<- tolower(classes$term)

##### match terms in dataSurvey to columns in sumTable
dataSumTableInSurvey <- match(termsFound,classes$term)

Standardize <- function(vals){
  m <- mean(vals,na.rm=TRUE)
  s <- sd(vals,na.rm=TRUE)
  std <- (vals - m)/s
  return(std)
}

classesNums<- levels(as.factor(classes$class))
par(mfcol=c(3,length(classesNums)))
intentsInSumTab <- list()
for(intent in classesNums){
  # isolate the trends to plot
  intentInSurvey <- which(classes$class == intent)
  intentFoundInSumTab <- intersect(intentInSurvey,dataSumTableInSurvey)
  intentInSumTab <- match(intentFoundInSumTab,dataSumTableInSurvey)
  toPlot <- sumTable[,intentInSumTab]
  intentsInSumTab[[intent]] <- intentInSumTab
  # plot
  plot(range(sumTable[,1]),c(0,500),type="n",
       xlab="Date (months)",ylab="Searches per month",
       main=intentions[as.numeric(as.character(intent))+1])
  for(iTerm in 1:dim(toPlot)[2]){
    lines(sumTable[,1],toPlot[,iTerm])
  }

  # normalize terms
  toPlotNorm <- apply(toPlot,2,Standardize)
  # plot normalized
  plot(range(sumTable[,1]),c(-2,8),type="n",xlab="Date (months)",ylab="Searches per month")
  for(iTerm in 1:dim(toPlot)[2]){
    lines(sumTable[,1],toPlotNorm[,iTerm])
  }

  # smooth# smooth
  nF<- 11
  toPlotSmooth <- apply(toPlotNorm,2,filter,rep(1/nF,nF))
  # plot smoothed
  plot(range(sumTable[,1]),c(-2,3),type="n",xlab="Date (months)",ylab="Searches per month")
  for(iTerm in 1:dim(toPlot)[2]){
    lines(sumTable[,1],toPlotSmooth[,iTerm])
  }
} 

#==========================
# Select terms for survey
#==========================
# At least n times non-zero search values
nMinNonZero <- 10
countNonZero <- function(vect){
    return(length(which(as.numeric(vect)!=0)))
}

toKeepColNums <- which(apply(sumTable[,-1],2,countNonZero)>=nMinNonZero)
print(names(sumTable[,toKeepColNums]))
toTrashColNums <- which(apply(sumTable[,-1],2,countNonZero)>=nMinNonZero)
windows()
par(mfrow=c(4,5))
for(i in (which(test==0)[-1]+1)){
    plot(sumTable[,1],sumTable[,i],xlab="time",ylab=names(sumTable)[i])
    lines(sumTable[,1],sumTable[,"exterminator"],col="blue")
}

countZero <- function(vect){
  return(length(which(as.numeric(vect)==0)))
}

nMaxZero <- 20
beginCountZero <- min(which((sumTable$beginTimes>=as.Date("2011-01-01"))==TRUE))
endCountZero <- max(which((sumTable$beginTimes<as.Date("2014-01-01"))==TRUE))
toTrashColumns <- which(apply(sumTable[beginCountZero:endCountZero,-1],
                              2,countZero)>nMaxZero)

#==========================
# statistical analysis
#==========================
# functions to ease the retrieval of slopes
GetSlope <- function(i,sumTable){
  s <- lm(sumTable[,i]~sumTable[,1])$coeff[2]
  names(s) <- colnames(sumTable)[i]
  return(s)
}
GetSlopes <- function(sumTable){
  slopes <- sapply(2:dim(sumTable)[2],GetSlope,sumTable,simplify=TRUE)
}
# fns for bootstrap on the slopes to know if any significant
library(boot)
GetMean<-function(dat,i,...){
  avg <- mean(dat[i])
  return(avg)
}
CustomBoot <- function(vect){
  if(length(vect)>0){
    outBoot <- boot(data=as.vector(vect),statistic=GetMean,R=nRepet,stype="i")
    CI<-boot.ci(outBoot,type=c("perc","bca"))
    out<-c(outBoot$t0,CI$percent[4:5],CI$bca[4:5])
  }else{
    out <-rep(NA,5)
  }

  return(out)
}
# just get the overall slope for each curve
overallSlopes <- GetSlopes(sumTable)

# get the slopes since 2011-01-01 and without the final 0
afterSpike <- which(sumTable[,1] >= as.Date("2011-01-01") & sumTable[,1] < as.Date("2014-01-01"))
afterSpikeSlopes <- GetSlopes(sumTable[afterSpike,])

# limit to terms that are significantly not null 
countZero <- function(vect){
  length(which(as.numeric(as.character(vect)) == 0))
}
# non null just after spike
nZeros <- apply(sumTable[afterSpike,],2,countZero)
# # non null in since 2008
# after2008 <- which(sumTable[,1] >= as.Date("2008-01-01") & sumTable[,1] < as.Date("2014-01-01"))
# nZeros <- apply(sumTable[after2008,],2,countZero)
#=> only 5 terms satisfy this criterium...

iBigTerms <- which(nZeros<10)

# average slope for each intent with bootstrap
avgSlopePostSpike <- list()
avgSlope <- list()
for(intent in classesNums){
  iIntent <- intentsInSumTab[[intent]]-1
  toKeep <- intersect(iIntent,iBigTerms)
  avgSlope[[intent]] <- CustomBoot(overallSlopes[toKeep])
  avgSlopePostSpike[[intent]] <- CustomBoot(afterSpikeSlopes[toKeep])
}

# simplify to an array the initial list
avgSlope <- simplify2array(avgSlope)
avgSlopePostSpike <- simplify2array(avgSlopePostSpike)

# plot by order of decreasing slope post spike
nL <- 4
nC <- 7
iCount <-1
for(iTerm in intersect(order(afterSpikeSlopes),iBigTerms-1)){
  if((iCount-1) %% (nL*nC)==0){
    dev.new()
    par(mfrow=c(nL,nC))
  }
  plot(sumTable[,1],sumTable[,iTerm+1],ylab=colnames(sumTable[iTerm+1]))
  iCount <- iCount +1
}


# permutation test on intentions: is the slope for each intention significantly different from the expected
# if randomly drawn from the available terms?
#--------------------------
# sample a thousand of distributions in the three/four groups
#   -> reorder the intentions but keep the labels
#--------------------------

# sample
avgS <- list()
avgSASS <- list()
for(i in 1:nRepet){
  scrambled<-sample(length(afterSpikeSlopes))
  sOA <- overallSlopes[scrambled]
  sAS <- afterSpikeSlopes[scrambled]
  for(intent in classesNums){
    iIntent <- intentsInSumTab[[intent]]-1
    toKeep <- intersect(iIntent,iBigTerms)
    avgS[[intent]][i] <- mean(sOA[toKeep])
    avgSASS[[intent]][i] <- mean(sAS[toKeep])
  }
}

# summarize as quantiles
qSlopes<-mat.or.vec(length(classesNums),2)
qSASS<-mat.or.vec(length(classesNums),2)
for(intent in classesNums){
  qSlopes[as.numeric(intent)+1,] <- quantile(avgS[[intent]],prob=c(0.025,0.975))
  qSASS[as.numeric(intent)+1,] <- quantile(avgSASS[[intent]],prob=c(0.025,0.975))
}

# barplot all the CI based measures together
# barplot with confidence intervals

barplot.ci<-function(y,yminus,ymax,ylim=c(min(y,yminus,ymax),max(y,yminus,ymax)),...){

  xbar<-barplot(y,ylim=ylim,...)

  errbar(xbar,y,yminus,ymax,add=TRUE,type="n")
  # may want to replace it with plotCI
  
  return(xbar)
}

dev.new()
par(mfcol=c(2,3))
barplot.ci(avgSlope[1,],avgSlope[2,],avgSlope[3,],main="Overall slope\nboot perc")
barplot.ci(avgSlope[1,],avgSlope[4,],avgSlope[5,],main="Overall slope\nboot bca")
barplot.ci(avgSlopePostSpike[1,],avgSlopePostSpike[2,],avgSlopePostSpike[3,],main="Post spike slope\nboot perc")
barplot.ci(avgSlopePostSpike[1,],avgSlopePostSpike[4,],avgSlopePostSpike[5,],main="Post spike slope\nboot bca")

# plot sampled versus observed
barplot.ci(avgSlope[1,],qSlopes[,1],qSlopes[,2],main="Overall slope\nresample null")
abline(h=mean(overallSlopes))
barplot.ci(avgSlopePostSpike[1,],qSASS[,1],qSASS[,2],main="Post spike slope\nresample null")
abline(h=mean(afterSpikeSlopes))