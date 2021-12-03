library(tidyverse)

flights <- read.csv("data/nyc_13_flights_small.csv") 
planes <- read.csv("data/nyc_13_planes.csv") 
weather <- read.csv("data/nyc_13_weather.csv") 

str(flights)
head(flights)
head(planes)

ab_join <- left_join(flights, planes)
abc_join <- left_join(ab_join, weather)
ab_join

#1
#Plot the departure delay of flights against the precipitation, and include a
#simple regression line as part of the plot

plot1 <- ggplot(abc_join, mapping = aes(x = dep_delay, y = precip))+
  geom_point() +
  geom_smooth(method='lm', formula = y~x) +
  theme_classic() 

  ggsave(filename = "data_outputs/plots/plot1.png", plot = plot1)

#2 Create a figure that has date on the x axis and each day’s mean departure 
#delay on the y axis. Plot only months September through December. Somehow 
#distinguish between airline carriers (the method is up to you). Again, save y
#our final product into the “plot” folder.

abc_join_filter <- abc_join %>% filter(month %in% 9:12) %>% group_by(month, day) %>% 
  summarise(mean_dep_delay = mean(dep_delay))
abc_join_filter %>% unite(date, month:day, sep = "-", remove = TRUE, na.rm = FALSE) %>%  
  ggplot(mapping = aes(x = date, y = mean_dep_delay))+
  geom_point()



