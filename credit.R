library(readxl)
library(plyr)
library(caret)
library(dplyr)
library(ROSE)
library(neuralnet)
library(Boruta)
library(ggplot2)
library(corrplot)

#!!The result might be different from the interpretation file because of handling imbalance and random select training and testing dataset
set.seed(2)

#Input Data
data=read_xlsx('C:/Users/Acer/OneDrive - Universitas Diponegoro/Ara/Techincal Test - Junior Data Scientist - Wahyu Tiara/Dataset_CreditScoring.xlsx')
data=as.data.frame(data)

#Preprocessing
#Missing Value
data=na.omit(data)

#Feature selection
fs=Boruta(TARGET~., data=data)
getSelectedAttributes(fs,withTentative = F)
fs.dat=attStats(fs)
data=data[,-c(2,7,11)]

#descriptive statistics
summary(data)


target=data[,1]
input=data[,-1]

#corelation plot
corrplot(cor(data), font.lab=5)


#Boxplot
par(mfrow=c(5,6), mar = c(1, 1, 1, 1))
for (i in 1:ncol(input)){
boxplot(unlist(input[,i]))
}

#Normalization
norm=function(inp){
  return((inp - min(inp, na.rm=TRUE)) / (max(inp, na.rm=TRUE) - min(inp, na.rm=TRUE)))
}
normal=as.data.frame(lapply(input, norm))

#Check boxplot after normalization
par(mfrow=c(5,6), mar = c(1, 1, 1, 1))
for (i in 1:ncol(normal)){
  boxplot((normal[,i]))
}

#making new dataset with normalized data
new.data=bind_cols(target, normal)
new.data=na.omit(new.data)
names(new.data)[1]='TARGET'

#Detect Data Imbalance
plyr::count(new.data$TARGET)
hist(target, col='#B1ABFF')

#Bcs we detect imbalance data so make it balance
balance=ovun.sample(TARGET~., data=new.data, method = 'over')
balance=balance$data
tab=table(balance$TARGET)
tab

#neuralnet
#split data into training and testing
index=as.data.frame(c(1:nrow(balance)))
i.data=bind_cols(index, balance)
names(i.data)[1]='idx'

training=sample_n(i.data, round(0.75*(tab[1]+tab[2])),0)
testing=subset(i.data, !(i.data$idx %in% training$idx))

training=as.data.frame(training[-1])
testing=as.data.frame(testing[-1])

#make model with training data
model=neuralnet(TARGET~., data=training, hidden=8, err.fct = 'sse',
                threshold=0.05,linear.output = TRUE, stepmax=1e+6,
                algorithm = 'rprop+')

model$result.matrix
plot(model)

#checking neural net accuracy with testing data
nn.res=compute(model,testing)
res=data.frame(actual=testing$TARGET, prediction=nn.res$net.result)
result=data.frame(ifelse(res>=0.5,1,0))
attach(result)
cm=table(actual,prediction)
cm
acc=(cm[1.1]+cm[2,2])/{cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2]}
acc

#predict random data
n.dat=c(1,1,0,7,4,125,3,3,5,7,14701,17312,10,0,0,3,4,0.849,0.667,0,0,1,0.583,0,0,0.714)
names(n.dat)=c('DerogCnt','CollectCnt','BanruptcyInd','InqCnt06','InqFinanceCnt24',
               'TLTimeFirst','TLTimeLast','TLCnt12','TLCnt24','TLCnt','TLSum','TLMaxSum','TLSatCnt',
               'TLDel60Cnt','TLBadCnt24','TL75UtilCnt','TL50UtilCnt','TLBalHCPct','TLSatPct',
               'TLDel3060Cnt24','TLDel90Cnt24','TLDel60CntAll','TLOpenPct','TLBadDerogCnt',
               'TLDel60Cnt24','TLOpen24Pct')
n.dat=t(n.dat)
pred=compute(model,n.dat)
n.res.pred=pred$net.result
result=ifelse(unlist(n.res.pred)>=0.5,1,0)
result
