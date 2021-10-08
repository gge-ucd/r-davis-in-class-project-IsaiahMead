##reading the data in
surveys <- read.csv("data/portal_data_joined.csv")
surveys

##a list of the column names 
colnames(surveys)

#selecting rows 1:60 and just columns 6, 9 and 13
surveys_new <- surveys[1:60, c(6, 9, 13)]
surveys_new

#converting factor data to character
surveys_new$species_id <- as.character(surveys_new$species_id)
surveys_new$plot_type <- as.character(surveys_new$plot_type)

#selecting only the ROWS that have complete cases (no NAs) **Notice the comma was needed for this to work**
surveys_new <- surveys_new[complete.cases(surveys_new), ]
surveys_new

#selecting just the weights (column 2) that are greater than 150
challenge_base <- surveys_new[(surveys_new[, 2]>150),]
challenge_base
