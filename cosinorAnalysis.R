#Seasonality analysis of Google Trends
#the key parameter is the one currently called timepoint, the estimate if positive suggests that the searches are increasing over time, controlling for the seasonality. 
#the other parametes cosw and sinw are harder to interpret, see: http://link.springer.com/chapter/10.1007/978-3-642-10748-1_3#page-1

# install.packages("season")
# uncomment if seasons necessary
library(season)

#### load the trends per term
#### this name may change according to your naming convention
#### this follows our current naming convention
load("requestResults/sumTable.Rda")

#the data is by month, and can be seen if you do edit(sumTable)

#make the dates work in R
#create a month and year variable
bedbugTerm = "bed bugs"
date<-as.Date(sumTable$Month,format='%y%m%d')
M<-as.numeric(factor(format(date,'%m')))
Year<-as.numeric(factor(format(date,'%y')))
#set term to one of the columns
term<-sumTable[,15]
#create a dataframe with Y and M
DATA<-data.frame(term,M)
#limit it to after january 2011
sel<-date>="2011-01-01"
DATA2<-DATA[sel==TRUE,]
DATA2$timepoint<-1:length(DATA2[,1])
names(DATA2)<-c("Y","MONTH","timepoint")
model<-cosinor(formula=Y~timepoint,date=MONTH,data=DATA2,type="monthly",family=poisson(link = "log"))

#set up for cosinor analysis with seasonality on the month and make an ever increasing number for months since start call it timepoint
#get column names of the sumTable
COLNAMES<-colnames(sumTable)

# COSINOR 
doCosinor<-function(keyword,pasteBedBugs=TRUE,sumTable,dateLimit="2011-01-01", bedbugTerm="bed bugs"){
  COLNAMES<-colnames(sumTable)
  toSearch<-keyword
  if(pasteBedBugs==TRUE){
  	toSearch<-paste(bedbugTerm,keyword)
  }
  	
  COLNUM<-which(COLNAMES==toSearch)

  if(length(COLNUM) == 0)
  	stop("The specified keyword doesn't exist. Try again.")
  				  
  date<-as.Date(sumTable$Month,format='%y%m%d')
  M<-as.numeric(factor(format(date,'%m')))
  Year<-as.numeric(factor(format(date,'%y')))
  				  
  #set the term being measured to one of the columns
  term<-sumTable[,COLNUM]
  #create a dataframe with term and M
  DATA<-data.frame(term,M)
  #limit it to after january 2011
  sel<-date>=dateLimit
  SELECTEDDATA<-DATA[sel==TRUE,]
  SELECTEDDATA$timepoint<-1:length(SELECTEDDATA[,1])
  names(SELECTEDDATA)<-c("TERM","MONTH","timepoint")
  model<-cosinor(formula=TERM~timepoint,date=MONTH,data=SELECTEDDATA,type="monthly",family=poisson(link="log"))
  								
  return(list(model=model, selData=SELECTEDDATA))
}

#KEY OUTPUTS
getEstimates<-function(model){
## AIC<-model[[2]][5]
## AIC not being used 
  COEF<-model[[2]][12]
  COEF2<-as.data.frame(COEF)
  ESTTREND<-COEF2[2,1]
  PTREND<-COEF2[2,4]
  STDERR<-COEF2[2,3]
  return(c(ESTTREND, PTREND, STDERR))
}

#this will make a basic plot 
plotCosinor<-function(model, selData){
  Y<-selData$Y
  MONTH<-selData$MONTH
  EST<-model$fitted.plus
  plot(Y)
  lines(EST)
  barplot(Y,space=0,names.arg=MONTH)
  lines(EST)
}

#this function will loop through the table
#and compute the p-values and models for every term
computeTable<-function(sumTable, bedbugTerm="bed bugs"){
  termCols<-which(grepl(bedbugTerm,colnames(sumTable)))
  savedModels<-list()
  termEstimates<-data.frame()	
  for(word in colnames(sumTable)[termCols]){
  	print(word)
  	savedModels[[word]]<-doCosinor(word,pasteBedBugs=FALSE,sumTable, bedbugTerm=bedbugTerm)
  	termEstimates<-rbind(termEstimates, getEstimates(savedModels[[word]]$model))
  }

  termEstimates <- as.matrix(termEstimates)
  colnames(termEstimates) <- c("estimatedTrend", "pValue", "stdError")
  rownames(termEstimates) <- colnames(sumTable)[termCols]

  return(list(savedModels=savedModels,termEstimates=termEstimates))
}

#save output--check the getEstimates STDERR looks like its the wrong column 
TRENDESTIMATES<-computeTable(sumTable, bedbugTerm=bedbugTerm)
TRENDESTIMATES$termEstimates

#pull in the survey
dat<-read.csv("Final_search_term_data.csv")

#get rid of blanks at end
dat1<-dat[-c(1:3)]
dat1<-dat[1:128,c(3:34)]
dat1<-dat[1:128,c(1:34)]

terms<-names(dat1)
terms<-terms[-c(1:4)]

##Note changing everything that was 1 and 3 etc to 999 manually)
#drop respondants who had 1 zero or 1 unclear response
isbad<-dat1[-c(1:4)]==0 | dat1[-c(1:4)]==999
numbad<-rowSums(isbad)
todrop<-which(numbad>0)  #lines needing dropping
dropUser<-dat$User[todrop]
droplist<-unique(dropUser)  #will drop 19

#drop others who visually saw had all 1s or all 4s etc
allbad<-c(13,25)
droplist<-c(droplist,allbad)
notdrop<-setdiff(1:64,droplist)

#makes dat2 dropping all the bad apples
dat2 <- dat1

for (i in droplist){
	dat2<- subset(dat2, dat2$User != i)
}

#create separate datasets for haves and havenots
#keeps 43 participants
nothave<-dat2[which(dat2$Have.==0),5:dim(dat2)[2]]
have<-dat2[which(dat2$Have.==1),5:dim(dat2)[2]]

#New normalization (have-not have)
norm<-have-nothave
final<-apply(norm,2,mean)
sort(final)


#===============Tabel 1 ########################
#meansd<-function(x){
#	temp<-mean(x)
#	temp2<-sd(x)
#	return(c(temp,temp2))
#}

#table<-apply(have,2,meansd)

#sum(have$Symptoms==i)

TABLE<-function(x)
{
	temp<-matrix(NA,30,4)
	for(i in 1:4)
	{
		for (j in 1:30)
		{
			temp[j,i]<-sum(x[,j]==i)
		}
	}
	return(temp)
}
THAVE<-TABLE(have)
TNOTHAVE<-TABLE(nothave)
TABLEALL<-data.frame(names(final), THAVE,TNOTHAVE)
names(TABLEALL)<-c("Term",1,2,3,4,1,2,3,4)
write.csv(TABLEALL,"TABLEALL.csv")


which(names(final)=="Itch")
#merge CAREFUL DOUBLE CHECK survey with trends 
if (bedbugTerm=="bedbugs"){
	todrop<-which(names(final)=="Itch")
	final<-final[-todrop]
	}

MERGED<-data.frame(final,TRENDESTIMATES$termEstimates)
plot(MERGED$estimatedTrend,MERGED$final,)
plot(rank(MERGED$final),rank(MERGED$estimatedTrend))

#correlation test
cor.test(MERGED$final,MERGED$estimatedTrend, method="spearman")  #doesnt work due to ties
#Kendalls tau-b is ok with ties; theres a warning on the output, but I think it can be ignored. 

cor.test(MERGED$final,MERGED$estimatedTrend, method="kendall")  #this outputs the tau-b which does work with ties. p-value might be not quite perfect.
#the alternative = "greater" is because were doing a one-sided test, we only are looking for a positive association
#this is the same as reporting the above with a cutoff of p<.10 though may be easier for readers to digest as people are dumb about pvalues
cor.test(MERGED$final,MERGED$estimatedTrend, method="kendall", alternative = "greater")  

#plot with terms 
par(mfrow=c(1,1),cex=1.4, cex.axis=1)
plot(MERGED$estimatedTrend,MERGED$final,col=0,xlab="Google Trend", ylab="Interview Score")
text(MERGED$estimatedTrend,MERGED$final,labels=names(final),cex=1/1.4)

par(mfrow=c(1,1),cex=1.4, cex.axis=1)
plot(MERGED$estimatedTrend,MERGED$final,col=0,xlab="Google Trend", ylab="Interview Score")
jittered<-MERGED$final
jittered[13]<-MERGED$final[13]-.025
text(MERGED$estimatedTrend,jittered,labels=names(final),cex=1/1.4)

#fix names for plot
NAMES<-names(final)

#alternative lets you identify just some terms, or all of them and avoid overlap--it puts term where you click
plot(MERGED$estimatedTrend,MERGED$final,cex=.3,col="grey",xlab="Google Trend", ylab="Interview Score")
identify(MERGED$estimatedTrend,MERGED$final,labels=terms)

#Michelle's code to get the amplitude & phase along with other parameters
getAllPara <- function(keyword){
  keyterm <- doCosinor(keyword, pasteBedBugs = TRUE, sumTable, dateLimit = "2011-01-01")
  info <- summary(keyterm$model)
  keyamp <- info$amp
  keyphase <- info$phase
  keyintercept <- keyterm$model$glm$coefficients["(Intercept)"]
  keytimepoint <- keyterm$model$glm$coefficients["timepoint"]
  keycosw <- keyterm$model$glm$coefficients["cosw"]
  keysinw <- keyterm$model$glm$coefficients["sinw"]
  keylow <- summary(keyterm$model)$lphase
  keyAIC <- keyterm$model[[2]]$aic
  keyinfo <- c(keyamp, keyphase, keylow, keycosw, keysinw, keytimepoint, keyintercept, keyAIC)
  return(keyinfo)
}

#put parameters into matrix 
termlist <- strsplit(COLNAMES[-1], " "); termmat <- matrix(,nrow = 30, ncol = 1)
for(j in 1:length(COLNAMES[-1])){
  termmat[j, 1] <- paste((termlist[[j]])[-(1:2)], collapse = " ")
}
allterms <- as.character(termmat)
para <- c("terms", "amplitude", "phase", "lowpt","cosw", "sinw", "timepoint", "intercept", 
          "AIC", "est A", "est P")
paramat <- matrix(, nrow = 30, ncol = 11)
colnames(paramat) <- para
paramat[,1] <- allterms
for(i in 1:30){
  coinf <- getAllPara(paramat[i,1])
  coinfmat <- as.matrix(coinf)
  paramat[i, 2] <- round(as.numeric(coinfmat[1,1]), digits = 4) #adding amplitude
  paramat[i, 3] <- coinfmat[2,1] #adding phase
  paramat[i, 4] <- coinfmat[3,1] #adding low point
  paramat[i, 5] <- round(as.numeric(coinfmat[4,1]), digits = 4) #adding cosw
  paramat[i, 6] <- round(as.numeric(coinfmat[5,1]), digits = 4) #adding sinw
  paramat[i, 7] <- round(as.numeric(coinfmat[6,1]), digits = 4) #adding timepoint
  paramat[i, 8] <- round(as.numeric(coinfmat[7,1]), digits = 4) #adding intercept
  paramat[i, 9] <- round(as.numeric(coinfmat[8,1]), digits = 4) #adding AIC 
}

#in case the estimated amplitudes and phases are also wanted
for(i in 1:30){
  c <- as.numeric(paramat[i, 5]) #cosw
  s <- as.numeric(paramat[i, 6]) #sinw
  
  paramat[i,10] <- round(sqrt((c^2) + (s^2)), digits = 4) #estimating amplitude
  
  estPhase = numeric()
  estPhase[i] <- if(c >= 0){  
    atan(s/c)
  } else {
    if (c < 0 & s > 0){
      atan(s/c) + pi
    } else {
      if (c < 0 & s > 0){
        atan(s/c) - pi
      } else {
        (-pi)/6     #I wasn't sure if the first condition also implied that s < 0. 
      }
    }
  }
  #since phase is on radians scale, transforming to time scale for monthly data
  #NOTE: if estP = 0, then conditions for it were not met! See above comment about condition.
  paramat[i,11] <- round(12*(estPhase[i]/(2*pi)) + 1, digits = 4)
}

#View(paramat) #uncomment to see the full matrix of parameters

#Code to graph for publication
pubtimes <- which(sumTable$Month >= "2011-01-01")
pubTable <- sumTable[pubtimes, ] #restricting data to after 2011
alltime <- merge(DATA2, pubTable, by = c("row.names")) #merging two data sets so I can get the correct dates
alltime <- alltime[,-c(1:2)] #getting rid of unneeded columns

alltime <- as.Date(alltime$Month)
grabJA <- subset(alltime$Month, format.Date(alltime$Month, "%m")=="01" | format.Date(alltime$Month, "%m")=="08")
grabJA <- sort(grabJA, by = "%y")

plotdat <- match(alltime$Month, grabJA)
plotdate <- which((is.na(plotdat) == FALSE))

#enter the six keyterms you want
keyvector <- c("exterminator", "hotels", "remedies", "city", "images", "about")
keymat <- matrix(, nrow = 3*45, ncol = (length(keyvector)))

#this saves all the necessary data vectors so we can use them to graph the plots
for(i in 1:length(keyvector)){
  colnames(keymat) <- keyvector
  
  keyCos <- doCosinor(keyvector[i], pasteBedBugs = TRUE, sumTable, dateLimit = "2011-01-01")
  x = keyCos$selData$timepoint
  y = keyCos$selData$TERM
  z = keyCos$model$fitted.plus
  
  keymat[1:45,i] <- x 
  keymat[46:90,i] <- y
  keymat[91:135,i] <- z
}

#this will create a 3x2 set of plots for the 6 chosen search terms
###NEED TO FIX AXES!!! (note to self) MIKE FIXED AXES BLEOW
#If you chose more than 6 search terms, change the numbers in par(mfrow = c(3,2))!
#png(file="three_by_two_plot.png", width=7, height=7)
par(mfrow=c(3,2), mai=c(.5,.3,.5,.5))  #mai lets uyou control the amount of blank space between plots see ?par
for(j in 1:length(keyvector)){
  xvec <- keymat[1:45,j]
  yvec <- keymat[46:90, j]
  zvec <- keymat[91:135, j]
  
  plot(xvec, yvec, xaxt = 'n', xlab = "", ylab = "Frequency", main = keyvector[j], xlim=c(0,45),ylim=c(0,400),cex.main=2)
  lines(xvec, zvec, col = "black")
  ###COMMENT LINE BELOW OUT FOR BLANK X AXIS
#axis(side = 1, at = x[plotdate], labels = grabJA, las = 2, cex.axis = 0.85)
#if(j==5|j==6)
#	{
		LABS<-c("Jan 11", "July 11", "Jan 12", "July 12", "Jan 13", "July 13", "Jan 14", "July 14" )
		AT<-sort(c(7,7+12*1:3, c(1,1+12*1:3)))
			axis(side = 1, at = AT, labels = LABS, las = 1, cex.axis = 1)
#	}
	}
#dev.off()