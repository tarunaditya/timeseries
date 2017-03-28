
library(fpp)
## timeseries data also available at http://robjhyndman.com/forecasting/data/
ts<-as.data.frame(as.numeric(windlts))
dim(ts)       ##168 rows
plot(ts[,1])  ## generally increasing, with seasonality


## data setup to use neural networks
lagpad <- function(x, k) {c(rep(NA, k), x)[1 : length(x)] }
lagging<-as.data.frame(matrix(0,nrow(ts),12))
for(i in 1:12){lagging[,i]<-lagpad(ts[,1],i)}
tsLagged<-cbind(ts,lagging,seq(1:nrow(ts)))
colnames(tsLagged)<-c("Windpower","l1","l2","l3","l4","l5","l6","l7","l8","l9","l10","l11","l12","monthNum")
tsLagged[1:16,]

library(h2o)
h<-h2o.init(nthreads = -1,max_mem_size = '8G')    
## load data into cluster
tsHex<-as.h2o(tsLagged,destination_frame = 'ts.hex')
## run deep learning against the time series: all but final year
dl<-h2o.deeplearning(x=c(2:14),y=1,training_frame = tsHex[1:464,],model_id = "tsDL",epochs = 1000,hidden=c(50,50))
summary(dl)
## predict final year
dlP<-h2o.predict(dl,newdata = tsHex[65:160,])
## plot 
plot(ts[1:168,1],type='l',main="H2O Deep Learning")
points(as.data.frame(dlP)[,1],x = 65:160,type='p',col="blue")

## quickly use forecast package to show what Arima will do
library(forecast)
myts <- ts(ts[,1], start=c(1950, 1), end=c(1989, 8), frequency=12)
fit <- stl(myts, s.window="period")
plot(fit)
autoArima<- auto.arima(window(myts, start=c(1950, 1), end=c(1989, 8)))
pAA<-forecast(autoArima,12)
plot(pAA)
pAA$model$series

#---------------------------------------------------------------------------------------------------------------------------------------------

library(data.table)
ts <- fread("~/Desktop/sp500.csv")
ts <- ts[order(nrow(ts):1),]
nrow(ts)
ts
Ttrain <- 1:16000
Ttest <- 16001:16649
head(ts[Ttest,])
ts <- ts$`Adj Close`
ts <- as.data.frame(ts)
plot(ts[,1],type='l',col="black")
lines(ts[Ttrain,1],type='l',col="blue")
lines(ts[Ttest,1],x=Ttest,type='l',col="red")

## data setup to use neural networks
lagpad <- function(x, k) {c(rep(NA, k), x)[1 : length(x)] }
lagging<-as.data.frame(matrix(0,nrow(ts),365))
for(i in 1:365){lagging[,i]<-lagpad(ts[,1],i)}
tsLagged<-cbind(ts,lagging,seq(1:nrow(ts)))
colnames(tsLagged) <- make.names(names(tsLagged))
colnames(tsLagged)<-c("target",paste0(c("l"),1:365),"dayNum")
#tsLagged

library(h2o)
h<-h2o.init(nthreads = -1,max_mem_size = '8G')    
## load data into cluster
tsHex<-as.h2o(tsLagged,destination_frame = 'ts.hex')
train <- tsHex[Ttrain,]
test <- tsHex[Ttest,]
## run deep learning against the time series: all but final year
dl<-h2o.deeplearning(x=c(2:ncol(tsHex)),y=1,training_frame = train,model_id = "tsDL",epochs = 100,hidden=c(50,50))

summary(dl)
## predict final year
dlP<-h2o.predict(dl,newdata = test)
## plot 
plot(ts[,1],type='l',col="black", main="H2O Deep Learning")
lines(as.data.frame(dlP)[,1],x = Ttest,type='l',col="blue",lw=5)

## quickly use forecast package to show what Arima will do
library(forecast)
myts <- ts(ts[,1], start=c(1950, 1), end=c(2016, 3), frequency=12)
fit <- stl(myts, s.window="period")
plot(fit)
autoArima<- auto.arima(window(myts, start=c(1950, 1), end=c(2013, 8)))
pAA<-forecast(autoArima,12)
plot(pAA)
pAA$model$series

