library(tidyverse)

#First, some notes...
#2 approachs for the same thing
gapminder %>% filter(year %in% c(2002,2007,2012))
gapminder %>% filter(year == 2002|year==2007|year==2012)

#these two arguments are essentially the same
gapminder %>% filter(year %in% c(2002))
gapminder %>% filter(year == 2002)

gapminder %>% filter(year %in% c(2002,2007)) %>%
  pivot_wider(id_cols = country,names_from = year,values_from = pop) %>%
  #column names that are numbers require a special character ``
  mutate(popDiff = `2007`-`2002`)

new_data = gapminder %>% filter(year %in% c(2002,2007)) %>%
  pivot_wider(id_cols = c(country,continent),names_from = year,values_from = pop) %>%
  mutate(popDiff = `2007`-`2002`) %>%
  filter(continent!='Oceania')

ggplot(new_data) + facet_wrap(~continent) + 
  geom_bar(aes(x = country,y = popDiff),stat = 'identity')

ggplot(new_data) + facet_wrap(~continent, scales = 'free') + 
  geom_bar(aes(x = reorder(country, popDiff),y = popDiff),stat = 'identity') + 
  labs(x = 'Country',y = 'change in pop. 2002 to 2007') + 
  theme(axis.text.x = element_text(colour = 'red',angle = 45)) + 
  coord_flip()

