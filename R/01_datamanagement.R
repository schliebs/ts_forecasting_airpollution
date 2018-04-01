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
  with(data,paste0(year,"-",month,"-",day," ",hour,":00:00")) %>% 
  strptime(.,format='%Y-%m-%d %H:%M:%S') %>% as.POSIXct()

names(data)

# Check missings

data$pm2.5 %>% is.na() %>% table()

# Delete first 24 hourse (all missings)
data %<>% filter(!No <= 24)

# Imputation 
library(mtsdi)

impvars <- ~hour + pm2.5 + DEWP + TEMP + PRES + cbwd + Iws + Is + Ir

imputed <- mnimput(impvars,
                   data,
                   eps=1e-3,
                   ts=TRUE, 
                   method="spline",
                   sp.control=list(df=c(7,7,7,7,7,7,7,7,7))) # no idea what dis does haha

summary(imputed)

data_imp <- imputed$filled.dataset

# now merge back or with old meta-data (NO, time, date etc. )





###

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

