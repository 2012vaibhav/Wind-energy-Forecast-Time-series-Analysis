
library('tidyr')
library(purrr)
library(dplyr)
library(data.table)
setwd("C:\\Users\\Vaibhav\\Desktop\\ADM CA2 Data New")
Temperature <- read.csv("Temperature Data.csv")

Temperature <- separate(data = Temperature, col = Date.Value.Anomaly, into = c("Date", "Temperature","DewPoint"), sep = "\\,")
Temperature <- extract(Temperature, Date, into = c("Year", "Month"), "(.{4})(.{2})", remove=FALSE)
#str(Temperature)
Temperature <- Temperature[,-c(1,5)]
Temperature$Temperature <- as.numeric(Temperature$Temperature)

#Convert fahrenheit to kelvin
Temperature$Temperature  <-  ((Temperature$Temperature - 32) * (5 / 9)) + 273.15
head(Temperature)
write.csv(Temperature,"Temperature.csv")


#reading Sea level pressure
setwd("C:\\Users\\Vaibhav\\Desktop\\ADM CA2 Data New\\Hourly Air Pressure") 
files <- list.files(pattern="*.csv")
AirPressure <- files %>% map_df(read.csv)

#Split the columns 
AirPressure <- separate(AirPressure,DATE,c("year","month"))
AirPressure$HourlySeaLevelPressure <- as.numeric(AirPressure$HourlySeaLevelPressure)
names(AirPressure)
typeof(AirPressure$HourlySeaLevelPressure)
sum(is.na(AirPressure$HourlySeaLevelPressure))
# Replaced NA values with mean of column
AirPressure$HourlySeaLevelPressure[is.na(AirPressure$HourlySeaLevelPressure)] <- round(mean(AirPressure$HourlySeaLevelPressure, na.rm = TRUE))
head(AirPressure)

#Convert pressure in to Pascal from hg
# 1 hg = 3386 pa
AirPressure$HourlySeaLevelPressure <- AirPressure$HourlySeaLevelPressure * 3386
dim(AirPressure)

write.csv(AirPressure,"Airpressure.csv")
# Apply this formulae  Airdensity <- AirPressure$HourlySeaLevelPressure / 287.05 * Temeprature
# Here 287.05 is constant

rm(list=ls(all=T))

library("forecast")

setwd("C:/Users/cheta/Desktop/")

powertimeseries <- read.csv("Dharshan Project.csv")
dim(powertimeseries) #204 *1

powertimeseries <- ts(powertimeseries$Power ,start = 2004,frequency = 12)
plot(powertimeseries)
powertimeseries
head(powertimeseries)
View(powertimeseries)
#Divides into Seasonal, Trend and Remainder. S.Window controls how rapidly the seasonal component can change
#STL means Seasonal Trend Decomposition using Loess
decom<-stl(powertimeseries,s.window="periodic")
decom
head(decom)
plot(decom)

########################
# HOLT-WINTERS Method
########################
hws<-HoltWinters(powertimeseries)

#Look at alpha and beta values
hws

head(hws$fitted)

plot(hws)
hws$SSE

# Calculating MAPE
power <- as.data.frame(powertimeseries)
head(power)
dim(power)

Pred_power <- as.data.frame(hws$fitted)
dim(Pred_power)
head(Pred_power)

MAPE_TSM <- mean(abs(power[1:171,] - Pred_power$xhat)/abs(power[1:171,]))*100



#Predict TES(Tripple Exponential Smoothing), Prediction interval gives me upper and lower bound of the confidence interval
Pred_power2<-predict(hws,n.ahead=12,prediction.interval=TRUE)

#Predicted values
Pred_power2

#Plot the base level graph, giving limits to x
plot.ts(powertimeseries, xlim = c(2004,2019))

#Historical Fitted values, now third column is trend. Xhat is prediction
hws$fitted

#Fit the historical fitted values
lines(hws$fitted[,1],col="green")

#Fit the future predicted values
lines(Pred_power2[,1],col="blue")

#Fit the upper interval predicted values
lines(Pred_power2[,2],col="red")

#Fit the upper interval predicted values
lines(Pred_power2[,3],col="red")

#Creating File for prediction
p<-predict(hws,12)


# Residual Analysis
residuals <- power[1:171,] - Pred_power$xhat

acf(residuals)
pacf(residuals)




#ARIMA
plot(powertimeseries)
powertimeseriesdiff1 <- 
  diff(milestimeseries, 
       differences=1)

milestimeseriesdiff1 <- ts(milestimeseriesdiff1,start = 2001,frequency = 12)
plot(milestimeseriesdiff1)
milestimeseriesdiff1

acf(milestimeseriesdiff1)
pacf(milestimeseriesdiff1)

milestimeseriesdiff2 <- 
  diff(milestimeseries, 
       differences=2)

milestimeseriesdiff2 <- ts(milestimeseriesdiff2,start = 2001,frequency = 12)
plot(milestimeseriesdiff2)
milestimeseriesdiff2

acf(milestimeseriesdiff2)
pacf(milestimeseriesdiff2)

# AUTO ARIMA(p,d,q)
powertimeseries_arima <- auto.arima(powertimeseries,ic='aic')
powertimeseries_arima


# Drift is nothing but Intercept
# arima(milestimeseries, c(1, 0, 1),seasonal = list(order = c(0, 1, 1), period = 12))

powertimeseries_arima$fitted
dim(powertimeseries_arima$fitted)

# MAPE Calculation
MAPE <- abs(as.data.frame(powertimeseries) - as.data.frame(powertimeseries_arima$fitted))/abs(as.data.frame(powertimeseries))
dim(MAPE)
class(MAPE)

MAPE <- mean(MAPE$x)*100


par(mfrow = c(1, 2))

acf(powertimeseries_arima$residuals)
pacf(powertimeseries_arima$residuals)
# Box.test(milestimeseries_arima$residuals, lag=20, type="Ljung-Box")

par(mfrow = c(1, 1))
powertimeseriesforecasts <- forecast:::forecast.Arima(powertimeseries_arima, 
                                                      h=12)
plot(powertimeseriesforecasts)
powertimeseriesforecasts


MAPE_TSM
MAPE
