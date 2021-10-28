library(tidyverse)
surveys <- read_csv("data/portal_data_joined.csv")

# --------------------------------------
## You do not need to type this code -- this is how we get the live code working
source('functions/livestreamSetup.R')
livestreamSetup(password = 'vulcans',user = 'rdavis',port = 4040)

# --------------------------------------
library(tidyverse)
surveys <- read_csv("data/portal_data_joined.csv")

# EXPLORING the NA situation with conditional statements
## leaving the last "else" argument in case_when assigns the 'large' value to EVERYTHING else, including NAs

surveys %>%
  mutate(weight_cat = case_when(
    weight >= 20.00 ~ "small",
    weight > 20.00 & weight < 48.00 ~ "medium",
    T ~ "large"
  )) %>%
  select(weight, weight_cat) %>%
  filter(is.na(weight))


# leaving the last "else" argument in ifelse assigns the 'large' value to everything else, BUT DOES NOT INCLUDE NAs
surveys %>%
  mutate(weight_cat = ifelse(weight >= 20.00, "small",
                             ifelse(weight > 20.00 & weight < 48.00, "medium"
                                    ,"large"))) %>%
  select(weight, weight_cat) %>%
  filter(is.na(weight))

# specify the final argument in case_when()
surveys %>%
  mutate(weight_cat = case_when(
    weight >= 20.00 ~ "small",
    weight > 20.00 & weight < 48.00 ~ "medium",
    weight >= 48.00 ~ "large"
  )) %>%
  select(weight, weight_cat) %>%
  filter(is.na(weight))

# Manipulate surveys to create a new dataframe called surveys_wide with:
# 1. column for genus and a column named after every plot type (step 2)
# 2. each of these columns containing the mean hindfoot length of animals in that plot type and genus. So every row has a genus and then a mean hindfoot length value for every plot type. (step 1)
# 3. The dataframe should be sorted by values in the Control plot type column. (step 3)

# Step 1:
surveys2 <- surveys %>%
  filter(!is.na(hindfoot_length)) %>%
  group_by(plot_type, genus) %>%
  summarize(mean_hindfoot = mean(hindfoot_length))
surveys2

# Step 2 & 3:
surveys_wide <- pivot_wider(surveys2, names_from = "plot_type", values_from = "mean_hindfoot")
surveys_wide

# arrange
surveys_wide %>%
  arrange(Control)

surveys_wide %>%
  arrange(desc(Control))





# What if we wanted to reverse this back into the longer version of what we made before
?pivot_longer
surveys_reverse <- surveys_wide %>%
  pivot_longer(cols = c(Control:`Spectab exclosure`),
               names_to = "plot_type",
               values_to = "mean_hindfoot")
# cols:  which columns I want to pivot
# names_to: takes the column name and puts them into a column. What do you want to name the column of column names?
# values_to: takes the values from each of these columns cells. What do you want to name the column of cell values?


surveys_complete <- read_csv("data/portal_data_joined.csv") %>% 
  filter(complete.cases(.))

plot <- ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight))

ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_point(alpha = .1, mapping = aes(color = plot_type))

# plot types as panels
ggplot(surveys_complete, aes(x = species_id, y = weight)) +
  geom_point() +
  facet_wrap(~plot_type)

# don't like the theme
ggplot(surveys_complete, aes(x = species_id, y = weight)) +
  geom_point() +
  theme_classic()

#box plot 
ggplot(surveys_complete, aes(x = species_id, y = weight)) +
  geom_boxplot() +
  theme_classic()
?complete.cases()
