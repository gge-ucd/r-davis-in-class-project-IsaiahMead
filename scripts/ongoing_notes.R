# installing tidy verse
install.packages("tidyverse")

#loading package into a session (must do every time)
library(tidyverse)

#reading in data in a "tibble" (sp?)
survey_t <- read_csv("data/portal_data_joined.csv")
install.packages("dplyr")

#installing packages
install.packages("lubridate")

##Manipulating data, grabbing columns 
select(object_name, column_name_1, Column_name_2)

#grabbing rows
filter(object_name,column_name=='desired value')

##double =, aka ==, asks is this equal to that
# inverse filtering
filter(object_name,column_name !='desired value')

##selecting and filtering at the same time, R works from indie out with 
#functions, ie filter first, then select
Select(filter(object_name,column_name !='desired value'), column_name, column_name)

##piping
%>% #strings selecting and filter together, object goes first, don't need to retype
# eg 
new_object_name <- object-name %>% filter(genus!="neotoma")

##Mutate, creates new column
object_name %>% mutate(new_column_name = original-column_name)
# note!, must overwrite object to get new column to save
# mutate works like select and filter, doesn't require piping

## Summarize
#must group_by first to specify where you're looking, eg what column
surveys %>% group_by(sex) %>% 
  summarize(avg_weight = mean(weight,na.rn = T),med_weight = median(weight,na.rm = T))

#tally function, "how many of these are there in this group" must group first
surveys %>% group_by(sex) %>% tally()
  

# file mgmt
dir.create()
getwd()
identical()#input two objects separated by a comma

# file navigation
"" + tab 

# file details
length()
class()
str()
typeof()
head()
summary()

##need help?
#put a ? infront of a function and run it for more info


#ifelse function
# eg ifelse(test, yes, no)
ifelse(surveys$hindfoot_length<mean(surveys$hindfoot_length, na.rm = T), 'small', 'big')
#this can be nested in a mutate function to create a new column, and column name
mutate(surveys,hindfoot_size = ifelse(surveys$hindfoot_length<mean(surveys$hindfoot_length, na.rm = T), 'small', 'big'))

