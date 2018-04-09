source("R/00_packages.R")

data <- readRDS("data/data_imputed.rds") 
names(data)

#########

names(data)
variables <- c("hour","pm2.5","DEWP","TEMP","PRES","cbwd","Iws","Is","Ir")


##########

lala <- function(X, min, max) {
  X_std = (X - min(X)) / (max(X) - min(X))
  x_scaled = X_std * (max-min)+min
  return(x_scaled)
}

lalaBack <- function(X, min, max) {
  X_std = (X - min(X)) / (max(X) - min(X))
  x_scaled = X_std * (max-min)+min
 # x_scaled <- round(x_scaled, 3)
  return(x_scaled)
}


full <- 
  data %>%
  dplyr::select(one_of(variables))# %>% 
  #mutate_all(funs(lala(.,0,1))) #%>% 
 # mutate_all(funs(round(.,6)))

### Invert Scaling test
#fullback <- lalaBack(full$pm2.5, min(data$pm2.5), max(data$pm2.5))
#testback <- lalaBack(test$pm2.5, min(data$pm2.5), max(data$pm2.5))

train <- full[2:(365*24),]
test <- full[((365*24)+1):nrow(data),]

full <- bind_rows(train,test)

# 
# varmodel <- VAR(y = train , # Data with endogenous Vars 
#              p = 1, # Lag-Order
#              type="none", # Whatever dis does
#              season = NULL) # maybe hour/month seasonality 


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

# minmax-scale depvar




###
hour = factor(full$hour)
dummies = model.matrix(~hour)[,-1]

modeldata<- 
  full %>% 
  dplyr::select(-hour)

names(modeldata)



# var=VARselect(modeldata,lag.max=50)
# var


library(tsDyn)
names(modeldata)

VARRR <- lineVar(modeldata, 
                 lag=30, 
                 model = c("VAR"),
                 exogen = dummies)



VARRR
summary(VARRR)

toLatex(VECM.EG)
toLatex(summary(VARRR))
# options("show.signif.stars"=FALSE)
# toLatex(summary(VECM.EG), parenthese="Pvalue")
# options("show.signif.stars"=TRUE)

var1_residuals <- resid(VARRR)
var1_residuals

par(mfrow=c(1,1))
acf(var1_residuals[,1],lag.max = 30)
pacf(var1_residuals[,1],lag.max = 30)


preds_roll <- predict_rolling(VARRR, nroll=35039)
preds_rollTRUE <- preds_roll[["true"]]
preds_rollPRED <- preds_roll[["pred"]]

df <- data.frame(pred = preds_rollPRED$pm2.5,
                 true = preds_rollTRUE$pm2.5) %>% 
  mutate(id = 1:nrow(.))  %>% 
  mutate(error = abs(pred-true),
         sq_error = (pred-true)^2)

head(df)


#df_backscaled <- 
#  data %>%
#  dplyr::select(one_of(variables)) %>% 
#  mutate_all(funs(lala(.,min(data$xxxxx),1))) %>% ##keine ahnung wie ich hier das min/max von jeder zeile anspreche

t <- (lalaBack(df$true, 
              min(data$pm2.5[((365*24)+1):length(data$pm2.5)]), 
              max(data$pm2.5[((365*24)+1):length(data$pm2.5)])))

p <- (lalaBack(df$pred, 
               min(data$pm2.5[((365*24)+1):length(data$pm2.5)]), 
               max(data$pm2.5[((365*24)+1):length(data$pm2.5)])))


sqrt(mean((df$true-df$pred)^2))
sqrt(mean((t-p)^2))
mean(abs(t-p))

hist(df$true)

sd(df$true)
sqrt(mean((diff(df$true)^2)))

ddd <- data.frame(t = df$true,l = lag(df$true))
sqrt(mean((ddd$t-ddd$l)^2,na.rm = T))
