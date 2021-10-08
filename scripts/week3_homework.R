surveys <- read.csv("data/portal_data_joined.csv")
surveys

colnames(surveys)

surveys_new <- surveys[1:60, c(6, 9, 13)]
surveys_new

surveys_new$species_id <- as.character(surveys_new$species_id)
surveys_new$plot_type <- as.character(surveys_new$plot_type)

surveys_new <- surveys_new[complete.cases(surveys_new), ]
surveys_new

challenge_base <- surveys_new[(surveys_new[, 2]>150),]
challenge_base
