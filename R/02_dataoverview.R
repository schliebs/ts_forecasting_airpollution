# To play around

source("R/00_packages.R")

data <- readRDS("data/data_imputed.rds")

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

