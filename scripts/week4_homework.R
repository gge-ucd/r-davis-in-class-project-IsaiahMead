library(tidyverse)

#1
surveys <- read_csv("data/portal_data_joined.csv")
str(surveys)

#2
surveys %>% filter(weight > 30 & weight < 60)

#3
biggest_critters <- surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(species_id, sex) %>% 
  summarise(max_weight = max(weight))

biggest_critters %>% arrange(max_weight)

biggest_critters %>% arrange(desc(max_weight))

#4
surveys %>% 
  filter(is.na(weight)) %>% group_by(species) %>% 
  tally() %>% arrange(desc(n))

surveys %>% 
  filter(is.na(weight)) %>% 
  group_by(plot_id) %>% 
  tally() %>% 
  arrange(desc(n))

surveys %>% 
  filter(is.na(weight)) %>% 
  group_by(year) %>% 
  tally() %>% 
  arrange(desc(n))

#5
surveys_avg_weight <- surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(species_id, sex) %>% 
  mutate(avg_weight = mean(weight)) %>% 
  select(species_id, sex, weight, avg_weight)

surveys_avg_weight

#6
surveys_avg_weight <- surveys_avg_weight %>% 
  mutate(above_average = weight > avg_weight)

surveys_avg_weight
