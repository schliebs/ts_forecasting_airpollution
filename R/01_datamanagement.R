# No: row number 
# year: year of data in this row 
# month: month of data in this row 
# day: day of data in this row 
# hour: hour of data in this row 
# pm2.5: PM2.5 concentration (ug/m^3) 
# DEWP: Dew Point (â„ƒ) 
# TEMP: Temperature (â„ƒ) 
# PRES: Pressure (hPa) 
# cbwd: Combined wind direction 
# Iws: Cumulated wind speed (m/s) 
# Is: Cumulated hours of snow 
# Ir: Cumulated hours of rain 


data <- read.csv(file = "data/PRSA_data_2010.1.1-2014.12.31.csv")
head(data)

library(plyr)
library(tidyverse)
library(magrittr)

# TS
library(TSA)
library(forecast)


data$date <- 
  with(data,paste(year,month,day,sep = "-")) %>% 
  as.Date()

names(data)

ggplot(data,
       aes(x = date,
           y = pm2.5)) + 
  geom_line()

ggplot(data,
       aes(x = date,
           y = log(pm2.5))) + 
  geom_line()


# Check stationarity

data$logdif[2:nrow(data)] <- diff(log(data$pm2.5))

data$logdif [data$logdif %in% c(-Inf,Inf)] <- NA
data %<>% na.omit()


ts.plot(data$pm2.5)
adf.test(data$pm2.5)

acf(data$pm2.5)
pacf(data$pm2.5)


# With logdif
ts.plot(data$logdif)
adf.test(data$logdif)

acf(data$logdif)
pacf(data$logdif)


# Achtung: Imputation for NAs?
which(is.na(x))

