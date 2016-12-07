library(data.table)

rm(list=ls())
wd         <- "C:/Lim Min/NTU/Math/MH4900 Final Year Project/RCode"
setwd(wd)

dat <- read.csv("STI_Index.csv", header = TRUE)
dt<-data.table(dat)
dt[,Date:=as.POSIXct(Date,format='%Y-%m-%d')]

#Input is 10 previous values, 28 stock set, 
TPrev<-10 #number of previous values to consider
dt<-appendNextN(dt,"Close","Code","Date",TPrev+1,"C")
dt<-dt[complete.cases(dt),]
datename<-unique(dt[order(Date),Date])
codename<-unique(dt[order(Code),Code])
for (i in 1:length(codename)){
  datename<-datename[datename %in% dt[Code==codename[i],Date]]
} #datename is list of dates that have all 28 stocks and 11 forward closing values
dtdata<-dt[Date %in% datename,]
TrainNum<-floor(0.6*length(datename))
reorderedDates<-sample(datename)
trainDates<-reorderedDates[1:TrainNum]
testDates<-reorderedDates[TrainNum:length(reorderedDates)]

trainData<-dtdata[Date %in% trainDates,]
testData<-dtdata[Date %in% testDates,]
meltTrainData<-melt(trainData,id.vars = c("Date","Code"),measure.vars = paste0("C",1:(TPrev+1)),
                    variable.name = "CVal", value.name = "CloseVal")
map<-data.table(CVal=paste0("C",1:(TPrev+1)),Cint=1:(TPrev+1))
useTrainData<-merge(meltTrainData,map,by="CVal")
useTrainData[,`:=`(Mean=mean(CloseVal),Sd=sd(CloseVal)),by=Code]
useTrainData[,`:=`(NormCloseVal=(CloseVal-Mean)/Sd)]


NStocks<-length(unique(orderedTrainData[,Code]))
N1<-100 #neurons in layer 1
N2<- 50 #neurons in layer 2
LRate<-0.001 #Learning Rate

W1 <- matrix(runif(N1*NStocks*TPrev,min=-0.1, max=0.1),nrow=N1,ncol=NStocks*TPrev) #100x280 matrix
Y1<-rep(0.0,N1)
W2 <- matrix(runif(N2*N1,min=-0.1, max=0.1),nrow=N2, ncol=N1) #50x100 matrix
Y2<-rep(0.0,N2)
W3 <- matrix(data=runif(NStocks*N2,min=-0.1, max=0.1),nrow=NStocks,ncol=N2,byrow=TRUE) #28x50 matrix
Y3<-rep(0.0,NStocks)

maxEpoch <- 30
rmsErr <- rep(0.0,maxEpoch)
for (i in 1:maxEpoch){
  print(i)
  for (j in 1:length(trainDates)){
    orderedTrainData<-useTrainData[Date==trainDates[j]&Cint<(TPrev+1),]
    orderedTrainData<-orderedTrainData[order(Code,Cint),]
    orderedOutputData<-useTrainData[Date==trainDates[j]&Cint==(TPrev+1),]
    orderedOutputData<-orderedOutputData[order(Code),]
    vecTrainData<-orderedTrainData[,NormCloseVal]
    vecOutputData<-orderedOutputData[,NormCloseVal]
    stopifnot(length(vecOutputData)==NStocks)
    # for (k in 1:N1){
    #   Y1[k]<-unisigmoidf(sum(W1[k,]*vecTrainData)) #100 elements
    # }
    Y1<-unisigmoidf(apply(W1,1,function(x) sum(x*vecTrainData)))
    # for (k in 1:N2){
    #   Y2[k]<-unisigmoidf(sum(W2[k,]*Y1)) # 50 elements
    # }
    Y2<-unisigmoidf(apply(W2,1,function(x) sum(x*Y1)))
    # for (k in 1:NStocks){
    #   Y3[k]<-sum(W3[k,]*Y2) # 28 elements
    # }
    Y3<-apply(W3,1,function(x) sum(x*Y2))
    D3 <- vecOutputData-Y3 #vector of 28 elements
    D2 <- (apply(W3, 2, function(x) sum(D3*x)))*Y2*(1-Y2) #vector of 50 elements
    D1 <- (apply(W2, 2, function(x) sum(D2*x)))*Y1*(1-Y1) #vector of 100 elements
    W1 <- W1 + LRate*(D1%*%t(vecTrainData))
    W2 <- W2 + LRate*(D2%*%t(Y1))
    W3 <- W3 + LRate*(D3%*%t(Y2))
    rmsErr[i]<-rmsErr[i]+sum(D3*D3)
  }
  rmsErr[i]<-sqrt(rmsErr[i]/(NStocks*length(trainDates)))
  if (rmsErr[i]<0.1){ # error below 0.1
    break
  }
}
save(trainData,testData,W1,W2,W3,rmsErr,file = "20163008.RData")


#Input is 10 previous values, 28 stock set, 
TPrev<-10 #number of previous values to consider
subdt<-dt[X>=0&X<TPrev,]
out<-dt[X==TPrev,.(Close,Code)]
subdt[,`:=`(MaxClose=max(Close),MinClose=min(Close)),by=Code]
subdt[,`:=`(NormVal=((Close-MinClose)/(MaxClose-MinClose))),by=Code]
all<-merge(subdt,out, by.x="Code", by.y="Code")
all[,`:=`(Output=((Close.y-MinClose)/(MaxClose-MinClose))),by=Code]

NStocks<-length(unique(subdt[,Code]))
N1<-100 #neurons in layer 1
N2<- 50 #neurons in layer 2

W1 <- array(runif(N1*NStocks*TPrev,min=-0.1, max=0.1),dim = c(N1, NStocks*TPrev))
Y1<-rep(0.0,N1)
W2 <- array(runif(N2*NStocks*TPrev,min=-0.1, max=0.1),dim = c(N2, NStocks*TPrev))
Y2<-rep(0.0,N2)

MaxEpoch<-100 #Set max number of epoch
EpochNum<-1


appendNextN<-function(dt,retColName,stockColName,dateColName,n,nextBaseName){
  #retColName is the column name that you want to lag.
  #Appends n cols, nextBaseName1, nextBaseName2,... nextBaseNamen
  #Dates must be yyyy-mm-dd
  dt<-dt[order(get(stockColName),get(dateColName)),]
  datename<-unique(dt[order(get(dateColName)),dateColName,with=FALSE])
  for (i in 1:n){
    colname<-paste0(nextBaseName,i)
    dt[,(colname):=getNAheadReturn(datename,get(retColName),get(dateColName),i),by=get(stockColName)]
  }
  return(dt)
}
getNAheadReturn<-function(datename,retcol,datecol,n){
  #Dates must be yyyy-mm-dd
  #Obtained the nth month return. Consider the case where for a particular stock,
  # only months 1,2,4,5 have forward return values a,b,c,d. 
  # Then if n=2, b,NA,d,NA will be returned, if n=3, NA,c,NA,NA will be returned
  if (n==1){
    return(retcol)
  }
  a<-match(format(datecol,"%Y-%m-%d"),format(datename,"%Y-%m-%d"))
  a<-a+n-1
  daten<-datename[a]
  b<-match(format(daten,"%Y-%m-%d"),format(datecol,"%Y-%m-%d"))
  return(retcol[b])
}

unisigmoidf<-function(x){
  return(1/(1+exp(-x)))
}