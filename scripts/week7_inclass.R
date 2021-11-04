library(tidyverse)
install.packages("BrailleR",type = )

library(ggplot2)
my_plot <- ggplot(diamonds, aes(x = clarity, fill = cut)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle=70, vjust=0.5)) +
  scale_fill_viridis_d(option = "C") +
  theme_classic()

library(BrailleR)
VI(myplot)

test = ggplot(data = gapminder,aes(x = lifeExp,y=gdpPercap))   +
  geom_smooth(aes(group = continent))
sonify::sonify(test)
library(sonify)
plot(iris$Petal.Width,iris$Sepal.Length)
