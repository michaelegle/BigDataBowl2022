library(tidyverse)
library(ggthemes)

#this is sample code for how to make the 2d Contour plot of punt locations. 
#out of laziness i only look at 2018, but should be fairly obvious how to expand this

track_2018 <- read.csv('tracking2018.csv') %>% filter(displayName == 'football')

punts <- plays %>% 
  filter(specialTeamsPlayType == 'Punt') %>% 
  select(gameId, playId, specialTeamsPlayType, specialTeamsResult)

track_2018 %>% 
  left_join(punts) %>% 
  filter(!is.na(specialTeamsResult)) %>% 
  filter(event %in% c('punt_land','punt_received','fair_catch')) %>% 
  mutate(x = ifelse(playDirection == "left", 120-x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y)) %>% 
  ggplot()+
  geom_density_2d_filled(mapping = aes(x = x, y = y))+
  theme_fivethirtyeight()+
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = 'none',
    plot.title = element_text(hjust = 0.5)
  )+
  labs(
    title = 'your title',
    caption = 'caption'
  )

ggsave('filename.png', height = 7, width = 13)
