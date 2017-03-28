# Reading data from csv file
wind<-read.csv('/home/tarun/Desktop/ipredict_models/WindTimeseries.csv',header = TRUE)
windts<-ts(wind[,2])

windl<-read.csv('/home/tarun/Desktop/ipredict_models/wind_longterm.csv',header = TRUE)
windlts<-ts(windl[,2])
plot.ts(windlts)

# checking out the smoothened timeseries
plot.ts(SMA(windlts,n=4))

windltimeseries <- ts(windlts, frequency=12, start=c(1987,1))
#spliting the frequency (12months) separated timeseries into seasonal, trending, noise component
windtimeseriescomponents <- decompose(windltimeseries)

#another way to split into components -STL (seasonal trends long term) using loess -local weighted regression scatterplot smoothing- 
#good with outliers & X-12 assumptions of aRIMA
stlfit<-stl(windltimeseries,t.window=15,s.window="periodic",robust=TRUE)
plot(naive(seasadj(stlfit)))
#Randomwalk
plot(forecast(stlfit,method="naive"))
#extract only the trending component as the seasonal component has no entropy/information/i.e there is no upredictable component in seasonal parts
windltimeseriesseasonallyadjusted <- windltimeseries - windtimeseriescomponents$seasonal

# Forecasting the trending component
windltimeseriesseasonallyadjustedforecasts <- HoltWinters(windltimeseriesseasonallyadjusted, beta=FALSE, gamma=FALSE)
# visualizing the forecasts of trending component
plot(windltimeseriesseasonallyadjustedforecasts)
plot(fitted(windltimeseriesseasonallyadjustedforecasts))

# Forecasting the entire timeseries
windltimeseriesforecasts <- HoltWinters(windltimeseries, beta=FALSE, gamma=FALSE)
# Visualizing the entire timeseries forecasts
plot(windltimeseriesforecasts)
plot(fitted(windltimeseriesforecasts))

#Metrics to show how seasonally adjusted forecasting reduces SSE by 300%
windltimeseriesforecasts$SSE
windltimeseriesseasonallyadjustedforecasts$SSE

# Using Holtwinters in 'forecast' package to foresee the next 'n' series of forecast... for now doing it for n=8
windltimeseriesforecasts8 <- forecast.HoltWinters(windltimeseriesforecasts, h=8)
windltimeseriesseasonallyadjustedforecasts8 <- forecast.HoltWinters(windltimeseriesseasonallyadjustedforecasts, h=8)
plot(windltimeseriesforecasts8)
plot.forecast(windltimeseriesforecasts8)

#correlogram 
#If the predictive model cannot be improved upon, there should be no correlations between forecast errors for successive predictions. 
#In other words, if there are correlations between forecast errors for successive predictions, it is likely that the simple exponential 
#smoothing forecasts could be improved upon by another forecasting technique.

#To figure out whether this is the case, we can obtain a correlogram of the in-sample forecast errors for lags 1-20. 
#We can calculate a correlogram of the forecast errors using the “acf()” function in R.
#To specify the maximum lag that we want to look at, we use the “lag.max” parameter in acf().
acf(windltimeseriesforecasts8$residuals, lag.max=40,na.action = na.pass)
pacf(windltimeseriesforecasts8$residuals, lag.max=40,na.action = na.pass)

# slight correlation exists so
#To test whether there is significant evidence for non-zero correlations at lags 1-20, we can carry out a Ljung-Box test
Box.test(windltimeseriesforecasts8$residuals, lag=20, type="Ljung-Box")
#plot residuals
plot.ts(windltimeseriesforecasts8$residuals)

#The plot shows that the in-sample forecast errors seem to have roughly constant variance over time
#To check whether the forecast errors are normally distributed with mean zero, we can plot a histogram of the forecast errors,
#with an overlaid normal curve that has mean zero and the same standard deviation as the distribution of forecast errors
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors,na.rm=TRUE)/4
  mysd   <- sd(forecasterrors,na.rm=TRUE)
  mymin  <- min(forecasterrors,na.rm=TRUE) - mysd*5
  mymax  <- max(forecasterrors,na.rm=TRUE) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins,na.rm=TRUE)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins,na.rm=TRUE)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(windltimeseriesforecasts8$residuals)
#holts smoothing- model2
windltimeseriesforecasts_holtssmoothing <- HoltWinters(windltimeseries, gamma=FALSE)
plot(windltimeseriesforecasts_holtssmoothing)

#skipping exponential smoothing via box-cox

#new model
#ARIMA models are defined for stationary time series. Therefore, if you start off with a non-stationary time series, you will first need to ‘difference’ the time series until you obtain a stationary time series.
windltimeseriesdiff1 <- diff(windltimeseries, differences=1)
plot.ts(windltimeseriesdiff1)
windltimeseriesarima <- arima(windltimeseries, order=c(0,1,1))
windltimeseriesarimaforecasts <- forecast.Arima(windltimeseriesarima, h=5)
plot(windltimeseriesarimaforecasts)
acf(windltimeseriesarimaforecasts$residuals, lag.max=20)
Box.test(windltimeseriesarimaforecasts$residuals, lag=20, type="Ljung-Box")
plot.ts(windltimeseriesarimaforecasts$residuals)
plotForecastErrors(windltimeseriesarimaforecasts$residuals)
mean(windltimeseriesarimaforecasts$residuals)
mean(windltimeseriesforecasts$residuals)
