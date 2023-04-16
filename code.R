library(TSA)
library(fUnitRoots)
library(urca)
library(forecast)

rm(list=ls())
getwd()
setwd("E:/2021春季学期/时间序列分析/作业/大作业")
data1=read.csv("data_inland.csv",header=F)
colnames(data1)=c("Domestic Passenger Traffic")
head(data1)
series1=ts(data1,start=c(2006,2),freq=12)
tsp(series1)
t<-time(series1)
t[which.min(series1)]
series1[which.min(series1)]

win.graph(width=7,height=3)
plot(series1,type='o',main="Domestic Passenger Traffic",ylab="Amount")
#ADF检验
summary(ur.df(log(series2),type="drift",lags =12,selectlags = "AIC") )
model0=lm(series2~time(series2))
detectAO(model1)
detectIO(model1)
#在2020年附近存在明显的异常值，先用前面的数据估计模型
series2=window(series1,end=c(2018,2))
plot(series2,type='o',main="Domestic Passenger Traffic",ylab="Amount")
win.graph(width=7,height=3)
Month=c("1","2","3","4","5","6","7","8","9","O","N","D")
plot(window(series1,end=c(2010,2)),main="Domestic Passenger Traffic",ylab="Amount")
points(window(series1,end=c(2010,2)),pch=Month)

#确定性趋势
month.=season(series2)
model1=lm(series2~time(series2)+month.)
summary(model1)
plot(rstudent(model1),xlab='Time',ylab='Standardized Residuals',
     main='Standardized Residuals of model1',type='o')

#得到平稳数据
plot(log(series2),type='o',main="log(Domestic Passenger Traffic)",ylab="Amount")
plot(diff(log(series2)),type='o',
     main="difference of log(Domestic Passenger Traffic)",ylab="Amount")
plot(diff(diff(log(series2)),lag=12),type='o',
     main="difference of log(Domestic Passenger Traffic) with season",ylab="Amount")
plot(diff(diff(log(window(series1,end=c(2010,2)))),lag=12),
     main="difference of log(Domestic Passenger Traffic) with seasond",ylab="Amount")
points(diff(diff(log(window(series1,end=c(2010,2)))),lag=12),pch=Month)
summary(ur.df(diff(diff(log(series2)),lag=12),type="drift",lags =12,selectlags = "AIC") )


#模型识别
win.graph(width=4,height=4)
acf(as.numeric(log(series2)),ci.type='ma',main='log(series2)',xaxp=c(0,20,10))
acf(as.numeric(diff(log(series2))),ci.type='ma',
    main='difference of log(series2)',xaxp=c(0,20,10))
acf(as.numeric(diff(diff(log(series2)),lag=12)),ci.type='ma',
    main='difference of log(series2) with season',xaxp=c(0,20,10))
acf(as.numeric(diff(diff(log(series2)),lag=24)),ci.type='ma',
    main='difference of log(series2) with season',xaxp=c(0,20,10))

pacf(as.numeric(log(series2)),main='log(series2)',xaxp=c(0,20,10))
pacf(as.numeric(diff(log(series2))),main='difference of log(series2)',xaxp=c(0,20,10))
pacf(as.numeric(diff(diff(log(series2)),lag=12)),main='difference of log(series2) with season',xaxp=c(0,20,10))
pacf(as.numeric(diff(diff(log(series2)),lag=24)),main='difference of log(series2) with season',xaxp=c(0,20,10))


eacf(diff(diff(log(series2)),lag=24))
ar(diff(diff(log(series2)),lag=24))

ar(series2)
res=armasubsets(series2,nar=13,nma=13)
win.graph(width=6,height=5)
plot(res)

ar(diff(diff(log(series2)),lag=24))

#模型参数估计
model2=arima(log(series2),order=c(2,1,0),seasonal=list(order=c(0,1,0),period=24))
model2
model3=arima(log(series2),order=c(0,1,2),seasonal=list(order=c(0,1,0),period=24))
model3
model4=arima(log(series2),order=c(1,1,2),seasonal=list(order=c(0,1,0),period=24))
model4
model5=arima(log(series2),order=c(2,1,2),seasonal=list(order=c(0,1,0),period=24))
model5
model6=arima(log(series2),order=c(2,1,1),seasonal=list(order=c(1,1,1),period=12))
model6
model7=arima(log(series2),order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12))
model7
model8=arima(log(series2),order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
model8

#模型诊断
win.graph(width=7,height=7)
tsdiag(model7)
tsdiag(model8)
win.graph(width=4,height=4)
qqnorm(residuals(model7))
qqline(residuals(model7))
shapiro.test(residuals(model7))
qqnorm(residuals(model8))
qqline(residuals(model8))
shapiro.test(residuals(model8))

#预测
win.graph(width=7,height=4)
result=plot(model7,n.ahead=24,type='b',xlab='Time',ylab='Amount',main='Predict form 2018.3 to 2020.2')
abline(h=coef(model7)[names(coef(model7))=='intercept'])
series3=window(series1,start=c(2018,3),end=c(2020,2))

forecast=result$pred
real=series3
predict=exp(forecast)
ci.up=exp(result$upi)
ci.down=exp(result$lpi)
cbind(real,ci.down,predict,ci.up)

#异常值
#干扰分析
model9=arimax(log(series1),order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12),
  xtransf=data.frame(p1=1*(seq(series1)==169),p2=1*(seq(series1)==169)),
  transfer=list(c(0,0),c(1,0)),method='ML')
model9

#异常值检验
detectAO(model9)
detectIO(model9)
m1=arimax(log(series1),order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12),
   xtransf=data.frame(p1=1*(seq(series1)==169),p2=1*(seq(series1)==169)),
   transfer=list(c(0,0),c(1,0)),method='ML',
   io=c(180))
detectAO(m1)
detectIO(m1)
m2=arimax(log(series1),order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12),
   xtransf=data.frame(p1=1*(seq(series1)==169),p2=1*(seq(series1)==169)),
   transfer=list(c(0,0),c(1,0)),method='ML',
   io=c(180),xreg=data.frame(AO=seq(series1)==181))
detectAO(m2)
detectIO(m2)
m3=arimax(log(series1),order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12),
   xtransf=data.frame(p1=1*(seq(series1)==169),p2=1*(seq(series1)==169)),
   transfer=list(c(0,0),c(1,0)),method='ML',
   io=c(180,171),xreg=data.frame(AO=seq(series1)==181))
detectAO(m3)
detectIO(m3)
m4=arimax(log(series1),order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12),
   xtransf=data.frame(p1=1*(seq(series1)==169),p2=1*(seq(series1)==169)),
   transfer=list(c(0,0),c(1,0)),method='ML',
   io=c(180,171,31),xreg=data.frame(AO=seq(series1)==181))
detectAO(m4)
detectIO(m4)

#影响何时会消失
win.graph(width=7,height=4)
p=1*(seq(series1)==169)
plot(ts(p*(0.0366)+ filter(p,filter=0.6419,method='recursive',side=1)*(-1.9037),
    frequency=12,start=c(2006,2)),xlim=c(2015,2025),type='h')
abline(h=0)
