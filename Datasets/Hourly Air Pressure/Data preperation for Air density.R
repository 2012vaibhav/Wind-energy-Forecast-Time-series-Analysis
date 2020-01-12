
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




