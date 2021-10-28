library(tidyverse)

survey <- read.csv("data/portal_data_joined.csv")

#Create a new data frame called surveys_base with only the species_id, the weight,
#and the plot_type columns.

surveys_base <- select(survey,species_id,weight,plot_type)
str(surveys_base)

#piping option
surveys_base <- survey %>% select(species_id, weight,plot_type)

#Have this data frame only be the first 60 rows.
surveys_base <- head(x = surveys_base,60)
surveys_base <- surveys_base[1:60,]
surveys_base <- survey[c(1:60), c(6,9,13)]

#Convert both species_id and plot_type to factors.
surveys_base$species_id <- as.factor((surveys_base$species_id))

#Explore these variables and try to explain why a factor is different from a character.
#Remove all rows where there is an NA in the weight column.

#good option, but omits ALL NAs
surveys_base_nonas <- na.omit(surveys_base)

#a better option
is.na(surveys_base$weight)

!is.na(surveys_base$weight)

surveys_base %>% na.omit(surveys_base$weight)
na.omit(surveys_base,surveys_base$weight)

surveys_base[is.na(surveys_base$weight),]
surveys_base[!is.na(surveys_base$weight),]

surveys <- survey
surveys[1,]

#CHALLENGE: Create a second data frame called challenge_base that only
#consists of individuals from your surveys_base data frame
#with weights greater than 150g.


challenge_base <- surveys_base[which(surveys_base$weight>150),]
summary(challenge_base$weight)

surveys_base$weight > 150
which(surveys_base$weight>150)

surveys_base[surveys_base$weight>150,] 

surveys <- read.csv('data/portal_data_joined.csv')
#1. keep only observations before 1995
surveys_base <- filter(surveys, surveys$year < 1995)

#keep year, sex and weight 
surveys_base <- select(surveys_base, year, sex, weight)
str(surveys_base)

surveys_base <- filter(surveys,year<1995) %>% select(year, sex, weight)
surveys_base <- surveys %>% filter(year<1995) %>% select(year,sex,weight)
surveys_base <- filter(select(surveys,year,sex,weight),year<1995)

#MUTATE f(x), creating a new column, or over writing existing columns

#Create a new data frame from the surveys data that meets the following criteria:
#1. contains only the species_id column
#2. and a new column called hindfoot_half containing values that are half the hindfoot_length values.
#3. In this hindfoot_half column, there are no NAs and all values are less than 30.
#Name this data frame surveys_hindfoot_half.

surveys %>% filter(!is.na(hindfoot_length)) %>%
  mutate(hindfoot_half = hindfoot_length/2) %>% select(species_id,hindfoot_half) %>%
  filter(hindfoot_half < 30)

#Use group_by() and summarize() to find the mean, min, and max hindfoot length
#for each species (using species_id).

#our try
surveys %>% group_by(species_id) %>% filter(!is.na(hindfoot_length)) %>%
  summarize(mean(hindfoot_length),min(hindfoot_length),max(hindfoot_length))

#instructor's example
surveys_hindfoot_half %>% group_by(species_id) %>%
  mutate(hindfoot_length = hindfoot_half * 2) %>%
  summarize(avg_length = mean(hindfoot_length),min(hindfoot_length),max(hindfoot_length)) %>%
  head()
