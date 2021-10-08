?set.seed(15)
hw2 <- runif(50, 4, 50)
hw2 <- replace(hw2, c(4,12,22,27), NA)
hw2

surveys <- read.csv("data/portal_data_joined.csv")

head(surveys, n=1)
str(surveys)
summary(surveys)


## subsetting from a data frame
surveys[1,2] #row 1, column 2

surveys$species

unique(surveys$species)

# nest functions
length(unique(surveys$species))


# levels as a way to identify unique characters factors,
#but this does not work for just characters
levels()

surveys200 <- surveys[200,]
surveys200

  