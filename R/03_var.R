source("R/00_packages.R")

data <- readRDS("data/data_imputed.rds")
names(data)

var <- VAR(y = data %>% dplyr::select(pm2.5,DEWP,TEMP,PRES), # Data with endogenous Vars 
             p = 2, # Lag-Order
             type="none", # Whatever dis does
             season = NULL) # maybe hour/month seasonality 

plot(var)



#Test for serial autocorrelation using the Portmanteau test
#Rerun var model with other suggested lags if H0 can be rejected at 0.05
serial.test(var, lags.pt = 10, type = "PT.asymptotic")

#ARCH test (Autoregressive conditional heteroscedasdicity)
arch.test(var, lags.multi = 10)

summary(var) #hm


#Forecasting
prd <- vars::predict(var, n.ahead = 1, ci = 0.95, dumvar = NULL)
print(prd)
plot(prd, "single")

View(prd$endog)
