library(TSA)
data(hours)
plot(hours)
data(wages)
plot(wages)
data(beersales)
plot(beersales)
data(winnebago)
plot(winnebago)
data(retail)
plot(retail)
data(prescrip)
plot(prescrip)
data(SP)
plot(SP)
data(JJ)
plot(JJ)
data(gold)
plot(gold)
data(deere1)
plot(deere1)
data(deere2)
plot(deere2)
data(deere3)
plot(deere3)
data(days)
plot(days)
data(boardings)
plot(boardings)
data(bluebirdlite)
plot(ts(bluebirdlite$price*10,start=c(2012,1),freq=12),ylab='price')
tsp(ts(bluebirdlite$price*10,start=c(2012,1),freq=12))
plot(ts(bluebirdlite$log.sales))
sales=ts(bluebirdlite$price*10,start=c(2012,1),freq=12)

data(robot)
plot(robot)
series=(robot+0.1)*1000
sales=ts(series,start=c(1994,1),freq=12)
#1993.1-2019.11
win.graph(width=7,height=3)
plot(sales,type='o',ylab='Sales',main='Sales')
tsp(sales)
mean(sales)
#趋势判断
#序列图不存在明显的季节性，季节差分效果差，推测不存在季节性
win.graph(width=7,height=3)
plot(window(sales,end=c(2000,1)),ylab='Sales',main='Sales')
points(window(sales,end=c(2000,1)),pch=as.vector(season(sales)))
plot(diff(log(sales),lag=12),ylab='Sales',main='Sales')

#一阶二阶差分效果都还不错
plot(log(sales),type='o',ylab='Sales',main='Sales')
plot(diff(log(sales)),type='o',ylab='Sales',main='Sales')
plot(diff(diff(log(sales)),lag=12),type='o',ylab='Sales',main='Sales')
plot(diff(log(sales),lag=12),type='o',ylab='Sales',main='Sales')
BoxCox.ar(sales)

#模型识别
win.graph(width=4,height=4)
acf(sales)
pacf(sales)
#通过acfpacf看出来非平稳，通过pacf可能识别AR（3）
eacf(sales)
#ARMA(1,1)
win.graph(width=6,height=6)
plot(armasubsets(sales,nar=12,nma=12,y.name='sales',ar.method='ols'))
#ar1,ma1,10

#模型参数估计
model1=arima(sales,order=c(1,0,0))
model1
model2=arima(sales,order=c(0,1,1))
model2
model3=arima(sales,order=c(1,0,1))
model3
#看一下参数是否显著，AIC检验

#模型诊断
res1=rstandard(model1)
res2=rstandard(model2)
res2=rstandard(model3)
win.graph(width=7,height=3)
plot(res1,ylab='AR(1) Residuals')
abline(h=0)
plot(res2,ylab='IMA(1,1) Residuals')
abline(h=0)
plot(res3,ylab='ARMA(1,1) Residuals')
abline(h=0)
acf(residuals(model1),main='AR(1) model',ylab='ACF of Residuals')
LB.test(model1)
acf(residuals(model2),main='IMA(1,1) model',ylab='ACF of Residuals')
LB.test(model2)
acf(residuals(model3),main='ARMA(1,1) model',ylab='ACF of Residuals')
LB.test(model3)
win.graph(width=4,height=4)
qqnorm(residuals(model1));qqline(residuals(model1))
shapiro.test(residuals(model1))
tsdiag(model1)
#参数冗余，有的模型识别过多参数，参数值不显著可以把模型排除

#模型预测
win.graph(width=7,height=3)
plot(model1,n.ahead=10)$pred
plot(model1,n.ahead=10)$upi
plot(model1,n.ahead=10)$lpi
plot(model3,n.ahead=10)$pred
plot(model3,n.ahead=10)$upi
plot(model3,n.ahead=10)$lpi

#干扰分析
#bluebirdlite
air.m1=arimax(log(airmiles),order=c(0,1,1),
   seasonal=list(order=c(0,1,1),period=12),
   xtransf=data.frame(I911a=1*(seq(airmiles)==69),
  I911b=1*(seq(airmiles)==69)),
  transfer=list(c(0,0),c(1,0)),
  xreg=data.frame(Dec96=1*(seq(airmiles)==12),
  Jan97=1*(seq(airmiles)==13),
  Dec02=1*(seq(airmiles)==84)),method='ML')




