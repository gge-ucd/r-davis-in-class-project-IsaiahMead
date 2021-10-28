## HOMEWORK FOR WEEK 4 REVIEW ##
library(tidyverse)

# 1. Create a tibble named surveys from the portal_data_joined.csv file.

surveys <- read_csv("data/portal_data_joined.csv")
# 2. Subset surveys to keep rows with weight between 30 and 60, and print out the first 6 rows.

filter(surveys, weight > 30 &weight < 60)

# cmd + shift + m to create pipe
surveys %>% filter(weight > 30 & weight < 60)

# How to print all columns of the data frame?
surveys %>% filter(weight > 30 & weight < 60) %>% head()
#we can use the View() function as well
View(surveys)
colnames(surveys)

# 3. Create a new tibble showing the maximum weight for each species + sex combination and name it biggest_critters. Sort the tibble to take a look at the biggest and smallest species + sex combinations.
biggest_critters <- surveys %>%
  filter(!is.na(weight) & !is.na(sex) & !is.na(species)) %>%
  group_by(species, sex) %>%
  summarize(maximum_weight = max(weight))

biggest_critters

# 4. Try to figure out where the NA weights are concentrated in the data

# what we proposed
surveys %>%
  filter(is.na(hindfoot_length)) %>% # get all the NAs in hindfoot
  group_by(species) %>%
  tally() #generates a column called "n", good to check how many, but it's a pipe dead end

# same as n
surveys %>%
  filter(is.na(hindfoot_length)) %>%
  group_by(species) %>%
  summarize(count = n(), mean = mean(weight, na.rm = T))

# saw use of sum(is.na()) for certain columns
sum(is.na(surveys&weight))

# colSums(is.na())
colSums(is.na(surveys))

# 5. Take surveys, remove the rows where weight is NA and add a column that contains the average weight of each species+sex combination to the full surveys dataframe. Then get rid of all the columns except for species, sex, weight, and your new average weight column. Save this tibble as surveys_avg_weight.

# group_by and mutate approach

# group_by and summarize approach. What is different?


# 6. Take surveys_avg_weight and add a new column called above_average that contains logical values stating whether or not a row’s weight is above average for its species+sex combination (recall the new column we made for this tibble).

# Conditional statements
# if(whatever){then this}

surveys %>% 
  filter(!is.na(weight)) %>%
  mutate(weight_cat = case_when(weight > mean(weight) ~ "big", 
                                weight < mean(weight) ~ "small"))

select(weight, weight_cat) %>% # select just take make our viewing better
  tail() # look at the bottom 6
# some used ifelse() here, which is a good segue...

# Using the iris data frame (this is built in to R), create a new variable that categorizes petal length into three groups:
#small (less than or equal to the 1st quartile)
#medium (between the 1st and 3rd quartiles)
#large (greater than or equal to the 3rd quartile)
#Hint: Explore the iris data using summary(iris$Petal.Length), to see the petal #length distribution. Then use your function of choice: ifelse() or case_when() to #make a new variable named petal.length.cat based on the conditions listed above. #Note that in the iris data frame there are no NAs, so we don’t have to deal with #them here.

data(iris)
summary(iris$Petal.Length)
str(iris)

iris %>% 
  mutate(Petal.Length.Cat = case_when(Petal.Length <= 1.6 ~ 'small', Petal.Length
  > 1.6 & Petal.Length<5.1 ~ 'medium', Petal.Length >= 5.1 ~ 'large')) 

# If Else Version
iris %>%
  mutate(length_cat = ifelse(Petal.Length <= 1.6, "small",
                             ifelse(Petal.Length >= 5.1, "large",
                                    "medium")))

# if the first statement is true, small, then go to nested. if nested is true, 
#go to large and if still not true, go to medium

#Joining Fx
#full join, joins everythind and keeps everything

tail_length <- read_csv('data/tail_length.csv')
str(tail_length)
str(surveys)

combo_dataframe <- left_join(surveys, tail_length, by = 'record_id')
str(combo_dataframe)

temp_df = surveys %>% group_by(year,plot_id) %>% tally()
temp_df %>% ungroup

pivot_wider(temp_df, names_from = 'year' ,values_from = 'n')


pivot_wider(data = surveys,id_cols = c('plot_id'),
            names_from = year,values_fn = n)
pivot_wider(temp_df,id_cols = 'plot_id',names_from = 'year',values_from = 'n')

?n_distinct
surveys %>% group_by(plot_id,year) %>% summarize(distinct_genus = n_distinct(genus))

surveys %>% group_by(plot_id,year) %>% summarize(length(unique(genus)))

#1
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_violin(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "tomato")
# adding violin
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_jitter(alpha = 0.1, color = "tomato") +
  geom_violin(alpha = 0)

ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_jitter(alpha = 0.3, color = "tomato") +
   geom_violin(alpha = 0) +
  theme_classic() +
  scale_y_log10()

#3
surveys_complete %>% filter(species_id == 'NL' | species_id =='PL') %>% 
ggplot(mapping = aes(x = species_id, y = hindfoot_length)) +
  geom_violin(alpha = 0) +
  geom_jitter(alpha = 0.3, mapping = aes(color = plot_id)) +
  theme_classic()

## plotis is numeric but want it to be categorical

hindfoot_survey <- surveys_complete %>%
  # inclusive is & vs "or" |
  filter(species_id == "NL" | species_id == "PF")

hindfoot_survey$plot_factor <- as.factor(hindfoot_survey$plot_id)

ggplot(data = hindfoot_survey,
       mapping = aes(x = species_id, y = hindfoot_length)) +
  geom_boxplot(alpha = 0.1) +
  geom_jitter(alpha = 0.3, mapping = aes(color = plot_factor))

surveys_complete %>%
  # inclusive is & vs "or" |
  filter(species_id == "NL" | species_id == "PF") %>%
  mutate(plot_factor = as.factor(plot_id)) %>%
  ggplot(mapping = aes(x = species_id, y = hindfoot_length)) +
  geom_boxplot(alpha = 0.1) +
  geom_jitter(alpha = 0.3, mapping = aes(color = plot_id))

surveys_complete %>%
  # inclusive is & vs "or" |
  filter(species_id == "NL" | species_id == "PF") %>%
  ggplot(mapping = aes(x = species_id, y = hindfoot_length)) +
  geom_boxplot(alpha = 0.1) +
  geom_jitter(alpha = 0.3, mapping = aes(color = as.factor(plot_id))) + 
  labs(x = "Species ID", y = "Hindfoot Length", title = "Boxplot", color = "Plot ID", theme_classic(), theme(axis.title.x = element_text(angle = 45))

       