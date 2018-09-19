library(tidyverse)
library(ggthemes)

rm(list = ls())

# import data
hellen <- read.table("./data/交友.txt") %>% 
  rename(flight = 'V1',
         game = 'V2',
         ice_cream = 'V3',
         friend_type = 'V4')


# plot
ggplot(data = hellen)+
  geom_jitter(aes(x= flight, y= game, color= as.factor(friend_type)))+
  theme_few()+
  geom_point(aes(x=10000, y=10))

# classify_friend_type
classify_friend_type(data = hellen, accuracy = 0.03, f=10000, g=10, i= 0.5)


# plot boxplot
hellen_to_one %>% 
  mutate(distance = sqrt(((10000 - flight_min)/flight_range - flight_to_one)^2 + ((10 - game_min)/game_range - game_to_one)^2 + ((0.5 - ice_cream_min)/ice_cream_range - ice_cream_to_one)^2)) %>% 
  ggplot(aes(x = as.factor(friend_type), y = distance, fill = as.factor(friend_type)))+
  geom_boxplot()+
  theme_few()

# plot
hellen_to_one %>% 
  ggplot(aes(x= flight_to_one, y= game_to_one, color= as.factor(friend_type)))+
  geom_jitter()+
  theme_few()
