library(ggplot2)
library(dplyr)
library(grid)

bethas <- read.csv('bethas.csv')
bethas %>% 
  ggplot(aes(x = cluster, 
             y = Betha)) + 
  geom_point() +
  geom_errorbar(aes(x = cluster, ymin = LB, ymax = UB, color = cluster),
                width = 0.05, 
                position = position_dodge(width = 0.3),
                size = 0.8) +
  
  facet_wrap(~index,
             ncol = 2, nrow = 5,
             scales = 'free') +
  
  labs(x = 'cluster',
       y = 'estimated β (95% CI)') + 
  theme_bw()