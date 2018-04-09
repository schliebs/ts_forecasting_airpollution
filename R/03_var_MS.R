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
  x_scaled <- round(x_scaled, 3)
  return(x_scaled)
}


full <- 
  data %>%
  dplyr::select(one_of(variables)) %>% 
  mutate_all(funs(lala(.,0,1))) %>% 
  mutate_all(funs(round(.,6)))

fullback <- lala(full$pm2.5, min(data$pm2.5), max(data$pm2.5))

test <- lala(full$pm2.5, min(full$pm2.5), max(full$pm2.5))

train <- full[1:(365*24),] 

test <- full[((365*24)+1):(nrow(data)-1),]

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


preds_roll <- predict_rolling(VARRR, nroll=35038)
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

head(lala(df$true, min(test$pm2.5), max(test$pm2.5)))
head(lala(df$true, min(data$pm2.5), max(data$pm2.5)))

library(ggplot2)
library(tidyverse)

preds_roll_for_scaling <- predict_rolling(VARRR, nroll=35039)
preds_roll_for_scalingTRUE <- preds_roll_for_scaling[["true"]]
preds_roll_for_scalingPRED <- preds_roll_for_scaling[["pred"]]
df_for_scaling <- data.frame(pred = preds_roll_for_scalingPRED$pm2.5,
                 true = preds_roll_for_scalingTRUE$pm2.5) %>% 
  mutate(id = 1:nrow(.))  %>% 
  mutate(error = abs(pred-true),
         sq_error = (pred-true)^2)

head(df_for_scaling)
head(lala(df_for_scaling$true, min(data$pm2.5), max(data$pm2.5)))




mean(df$pred - df$true)
mean((df$pred-df$true)^2)

sqrt(mean((df$pred-df$true)^2))

x = range(data$pm2.5)
backTRUE <- lala(X = df$true,x[1],x[2])
backPRED <- lala(X = df$pred,x[1],x[2])
mean((df$pred - df$true)^2) %>% sqrt()


########

dflong <- 
  df %>% 
  dplyr::select(-error,-sq_error) %>% 
  gather(var,value,-id)
library(hrbrthemes)

gg <- 
  ggplot(dflong %>%  filter(id < 100)) + 
  geom_line(aes(x = id,
                y = value,
                color = var, 
                alpha = var),
            size = 1.3) + 
  scale_alpha_manual(values = c("pred" = 1,
                                "true" = 1)) + 
  labs(x = "time step",
       y = "pollution",
       title = "Predicted vs. True pm2.5 Pollution values",
       subtitle = "Selected 100-day timeframe ")+
  theme_ipsum(grid = "Y");gg


ggsave("test.png",
       gg,
       height = 10,
       width = 16,
       dpi = 1000,
       device = "png")

##

names(df)
df2 <- df %>%  filter(id < 100)

gg1 <- 
  ggplot() + 
  geom_line(aes(x = 1:length(backPRED),
                y = backPRED),
            size = 1,
            color = "blue");gg1

gg2 <- 
  ggplot() + 
  geom_line(aes(x = 1:length(backTRUE),
                y = backTRUE),
            size = 1,
            color = "red");gg2


sqrt(mean((backTRUE-backPRED)^2))

