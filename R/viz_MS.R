library(ggplot2)
library(tidyverse)

dflong <- df %>% 
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


ggsave("test.pdf",
       gg,
       height = 10,
       width = 16,
       dpi = 2000,
       device = "pdf")

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
