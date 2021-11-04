library(tidyverse)
install.packages("ggthemes")
library(ggthemes)

#ONLY change the "data" part of this path if necessary
gapminder <- read_csv("https://gge-ucd.github.io/R-DAVIS/data/gapminder.csv") 

#1
gapminder %>%
  group_by(continent, year) %>% 
  summarize(mean_lifeExp = mean(lifeExp)) %>%
  
  ggplot() +
  geom_point(aes(x = year, y = mean_lifeExp, color = continent)) + 
  geom_line(aes(x = year, y = mean_lifeExp, color = continent))

#PROBLEM 2:


ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent, size = pop)) + 
  scale_x_log10() +
  geom_smooth(method = 'lm', color = 'black', linetype = 'dashed') +
  theme_bw()

#it appears  the scale_x_log argument effects the units of the x axis
#the geom_smooth function adds the dashed line of best fit

#PROBLEM 3

countries <- c("Brazil", "China", "El Salvador", "Niger", "United States")

gapminder %>% 
  filter(country %in% countries) %>% 
  ggplot(aes(x = country, y = lifeExp))+
  geom_boxplot() +
  geom_jitter(alpha = 0.3, color = "blue")+
  theme_tufte() +
  ggtitle("Life Expectancy of Five Countries") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Country") + ylab("Life Expectancy")
