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

source("R/00_packages.R")


data <- read.csv(file = "data/raw/PRSA_data_2010.1.1-2014.12.31.csv")
head(data)





data$date <- 
  with(data,paste0(year,"-",month,"-",day," ",hour,":00:00")) %>% 
  strptime(.,format='%Y-%m-%d %H:%M:%S') %>% as.POSIXct()

names(data)

# Check missings

data$pm2.5 %>% is.na() %>% table()

# Delete first 24 hourse (all missings)
data %<>% filter(!No <= 24)

# Imputation 

impvars <- ~hour + pm2.5 + DEWP + TEMP + PRES + cbwd + Iws + Is + Ir

imputed <- mnimput(impvars,
                   data,
                   eps=1e-3,
                   ts=TRUE, 
                   method="spline",
                   sp.control=list(df=c(7,7,7,7,7,7,7,7,7))) # no idea what dis does haha

summary(imputed)

data_imp <- imputed$filled.dataset
names(data) [!names(data) %in% names(data_imp)]

merged <- data.frame(data[names(data) [!names(data) %in% names(data_imp)]],
                     data_imp)

saveRDS(merged,"data/data_imputed.rds")

# now merge back or with old meta-data (NO, time, date etc. )



