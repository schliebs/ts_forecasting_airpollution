source("R/00_packages.R")

data <- readRDS("data/data_imputed.rds")
names(data)

#########



##########

train <- data[1:(365*24),]
test <- data[((365*24)+1):(nrow(data)-1),]

names(data)
variables <- c("pm2.5","DEWP","TEMP","PRES","cbwd","Iws","Is","Ir")

var <- VAR(y = train %>% dplyr::select(one_of(variables)), # Data with endogenous Vars 
             p = 1, # Lag-Order
             type="none", # Whatever dis does
             season = NULL) # maybe hour/month seasonality 

plot(var)
summary(var)



#Test for serial autocorrelation using the Portmanteau test
#Rerun var model with other suggested lags if H0 can be rejected at 0.05
serial.test(var, lags.pt = 10, type = "PT.asymptotic")

#ARCH test (Autoregressive conditional heteroscedasdicity)
arch.test(var, lags.multi = 10)

summary(var) #hm


#Forecasting
prd <- predict(var, n.ahead = 1, ci = 0.95, dumvar = NULL)
print(prd)
plot(prd, "single")

View(prd$endog)
