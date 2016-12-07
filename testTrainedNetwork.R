library(data.table)

rm(list=ls())
wd         <- "C:/Lim Min/NTU/Math/MH4900 Final Year Project/RCode"
setwd(wd)

load("20160830.RData")

meltTrainData<-melt(trainData,id.vars = c("Date","Code"),measure.vars = paste0("C",1:(TPrev+1)),
                    variable.name = "CVal", value.name = "CloseVal")
map<-data.table(CVal=paste0("C",1:(TPrev+1)),Cint=1:(TPrev+1))
useTrainData<-merge(meltTrainData,map,by="CVal")
useTrainData[,`:=`(Mean=mean(CloseVal),Sd=sd(CloseVal)),by=Code]
useTrainData[,`:=`(NormCloseVal=(CloseVal-Mean)/Sd)]

transformationConstants <- unique(useTrainData[order(Code),.(Code,Mean,Sd)])

meltTestData<-melt(testData,id.vars = c("Date","Code"),measure.vars = paste0("C",1:(TPrev+1)),
                    variable.name = "CVal", value.name = "CloseVal")
map<-data.table(CVal=paste0("C",1:(TPrev+1)),Cint=1:(TPrev+1))
useTestData<-merge(meltTestData,map,by="CVal")
useTestData<-merge(useTestData,transformationConstants,by="Code")
useTestData[,`:=`(NormCloseVal=(CloseVal-Mean)/Sd)]
actualOutputTestData<-useTestData[Cint==(TPrev+1),]

orderedTestData<-useTestData[Cint<(TPrev+1),]
orderedTestData<-orderedTestData[order(Date,Code,Cint),]
orderedTestData[,`:=`(NeuralOutput = runNeuralNet(NormCloseVal,Code,Cint,W1,W2,W3,TPrev)), by=Date]

subsetActualOutputTestData<-actualOutputTestData[,.(Code,Date,NormCloseVal)]
setnames(subsetActualOutputTestData,"NormCloseVal","ActualOutput")

subsetNeuralOuputTestData<-unique(orderedTestData[,.(Code,Date,NeuralOutput)])

allData<-merge(subsetActualOutputTestData,subsetNeuralOuputTestData,by=c("Code","Date"))

allData<-melt(allData,id.vars = c("Code","Date"),measure.vars = c("ActualOutput","NeuralOutput"),
                   variable.name = "OutputType", value.name = "Value")

library(ggplot2)
cPalette <- c("#000000", "#FF0000", "#E69F00", "#56B4E9", "#009E73",
              "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
              "#80FF00")

ggplot(allData, aes(x = Date, y = Value, label=Value)) + facet_wrap(~ Code,ncol=7)+
  geom_line(aes(group = factor(OutputType),colour = factor(OutputType))) + scale_colour_manual(values=cPalette) + 
  ylab("Value") + xlab("Date")
ggsave(file=paste0("Ouput.jpg"),
       width = 90, height = 45, units = "cm")


runNeuralNet<-function(StartVal,Code,Cint,W1,W2,W3,TPrev){
  dt<-data.table(Code,Cint,StartVal)
  # if(!all(dt[order(Code,Cint),]==dt)){
  #   return("Not in order")
  # }
  Y1<-unisigmoidf(apply(W1,1,function(x) sum(x*StartVal)))
  Y2<-unisigmoidf(apply(W2,1,function(x) sum(x*Y1)))
  Y3<-apply(W3,1,function(x) sum(x*Y2)) #vector of 28 elements)
  return(rep(Y3,each=TPrev))
}


unisigmoidf<-function(x){
  return(1/(1+exp(-x)))
}
