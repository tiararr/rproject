AirPassengers
t=1:length(AirPassengers)
y=AirPassengers
#membuat model
fit=lm(y~t)
summary(fit)
#membuat plot data
plot(t,y,type="o",xlab="monthly, 149-1960",ylab="AirPassengers")
abline(fit)

#exponential smoothing
#Holt winter aditiv
plot(co2) #data trend seasonal
model1=HoltWinters(co2)
fore1=predict(model1,50,prediction.interval = T)
plot(model1, fore1)
plot(fitted(model1))

#holt winter multiplikatif
plot(AirPassengers)
model2=HoltWinters(AirPassengers,seasonal="mult")
fore2=predict(model2,24,prediction.interval=T)
plot(model2,fore2)
plot(fitted(model2))

#membandingkan add dan multi
par(mfrow=c(1,1))
#Holt winter aditiv
plot(co2)
#holt winter multiplikatif
plot(AirPassengers)

#model smoothing holt
plot(uspop)
uspop
#data ditambahkan nilai error
x=uspop+rnorm(uspop,sd=5)
model3=HoltWinters(x, gamma=F)
model3$SSE
fore3=predict(model3,5,prediction.interval = T)
plot(model3, fore3)
plot(fitted(model3))

#smoothing exponential sederhana
x=uspop+rnorm(uspop,sd=5)
model4=HoltWinters(x,gamma=F,beta=F)
fore4=predict(model4,5,prediction.interval = T)
model4$SSE
plot(model4,fore4)
plot(fitted(model4))

#import data
library(readxl)
export=read_excel('d:/College/Komputasi Statistika/data smoothing.xlsx')
str(export)

#membentuk time series
expo_ts=ts(export$Export,start=c(2006,1), frequency =12)
plot(expo_ts)

#1. Additive Holt Winter
model1=HoltWinters(expo_ts)
pore1=predict(model1,25,prediction.interval = T)
plot(model1,fore1)
plot(fitted(model1))
model1$SSE
#2. Multiplicative Holt Winter
model2=HoltWinters(expo_ts,seasonal="mult")
pore2=predict(model2,25,prediction.interval = T)
plot(model2,fore2)
plot(fitted(model2))
model2$SSE
#3. Non-seasonal holt winter
#x=expo_ts+rnorm(expo_ts,sd=5)
x=expo_ts
model3=HoltWinters(x, gamma=F)
fore3=predict(model3,25,prediction.interval = F)
plot(model3, fore3)
plot(fitted(model3))
model3$SSE
#4.Exponential smoothing (sederhana)
#x=expo_ts+rnorm(expo_ts,sd=5)
x=expo_ts
model4=HoltWinters(x, gamma=F, beta=F)
fore4=predict(model4,25,prediction.interval = T)
plot(model4, fore4)
plot(fitted(model4))
model4$SSE
