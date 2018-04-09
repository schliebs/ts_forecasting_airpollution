source("R/00_packages.R")

data <- readRDS("data/data_imputed.rds")
names(data)

#########

names(data)
variables <- c("pm2.5","DEWP","TEMP","PRES","cbwd","Iws","Is","Ir")

data$date
xts(data,data$date)

##########
train <- data[1:(365*24),] %>% 
  dplyr::select(one_of(variables))

test <- data[((365*24)+1):(nrow(data)-1),]%>% 
  dplyr::select(one_of(variables))

full <- bind_rows(train,test)


varmodel <- VAR(y = train , # Data with endogenous Vars 
             p = 1, # Lag-Order
             type="none", # Whatever dis does
             season = NULL) # maybe hour/month seasonality 


# Modelfitting

adf.test(full$pm2.5)
adf.test(full$DEWP)
adf.test(full$TEMP)
adf.test(full$PRES)
adf.test(full$cbwd)
adf.test(full$Iws)
adf.test(full$Is)
adf.test(full$Ir)

##


library(tsDyn)
VARRR <- lineVar(full, 
                 lag=10, 
                 model = c("VAR"),
                 include = )
VARRR
summary(VARRR)

var1_residuals <- resid(VARRR)
var1_residuals
acf(var1_residuals)
par(mfrow=c(2,2))
acf(var1_residuals[,1])
acf(var1_residuals[,2])


preds_roll <- predict_rolling(VARRR, nroll=500)
preds_rollTRUE <- preds_roll[["true"]]
preds_rollPRED <- preds_roll[["pred"]]

library(ggplot2)
library(tidyverse)

df <- data.frame(pred = preds_rollPRED$pm2.5,
                 true = preds_rollTRUE$pm2.5) %>% 
  mutate(id = 1:nrow(.)) 

dflong <- df %>% 
  gather(var,value,-id)

gg <- 
  ggplot(dflong %>% filter(id < 20)) + 
  geom_line(aes(x = id,
                y = value,
                color = var, alpha = var),
            size = 1.3) + 
  scale_alpha_manual(values = c("pred" = 0.5,
                                "true" = 0.5));gg


ggsave("test.pdf",
       gg,
       height = 10,
       width = 16,
       dpi = 2000,
       device = "pdf")

mean(df$pred - df$true)
range(df$pred - df$true)

mean((df$pred - df$true)^2) %>% sqrt()




#Test for serial autocorrelation using the Portmanteau test
#Rerun var model with other suggested lags if H0 can be rejected at 0.05
serial.test(VARRR, lags.pt = 10, type = "PT.asymptotic")

#ARCH test (Autoregressive conditional heteroscedasdicity)
arch.test(var, lags.multi = 10)

summary(var) #hm


#Forecasting
prd <- predict(var, n.ahead = 1, ci = 0.95, dumvar = NULL)
print(prd)
plot(prd, "single")

View(prd$endog)
