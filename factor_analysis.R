library(forecast)
library(FNN)
library(rpart)
library(dplyr)
library(rpart.plot)
library(randomForest)
library(stringr)

house_df <- read.csv("./data/zillow_data.csv", stringsAsFactors = FALSE)

###############################################################################
# Convert categorical data to numeric values
house_df$Basement <- ifelse(house_df$Basement == "Finished", 1, 0)
house_df$Townhouse <- ifelse(house_df$Home.Type == "Townhouse", 1, 0)
house_df$Single.Family <- ifelse(house_df$Home.Type == "SingleFamily", 1, 0)
house_df$City.View <- NA
house_df$Mountain.View <- NA
house_df$Territorial.View <- NA
house_df$Water.View <- NA

# Identify the number of views of the house
for (i in 1:nrow(house_df)) {
  house_df[i, "View"] <- (str_count(house_df[i, "View"], ",") + 1)
}

# Identify the number of heating sources
for (i in 1:nrow(house_df)) {
  house_df[i, "Heating.Sources"] <- 
    (str_count(house_df[i, "Heating.Sources"], ",") + 1)
}

# Identify if the house has a garage
house_df$Garage <- NA
for (i in 1:nrow(house_df)) {
  if (grepl("Garage", house_df[i, "Parking"]) == TRUE) {
    house_df[i, "Garage"] <- 1
  } else {
    house_df[i, "Garage"] <- 0
  }
}

# Identify the number of heating system for the house
for (i in 1:nrow(house_df)) {
  house_df[i, "Heating.System"] <- 
    (str_count(house_df[i, "Heating.System"], ",") + 1)
}

# Remove original columns of categorical data and irrelevant columns
model_df <- house_df %>% select(Zestimate, Last.Sold.Price, Year.Built, 
                                Floors, Basement, Bathrooms, Bedrooms, 
                                Finished.SQFT, Lot.Size.SQFT, Townhouse, 
                                Single.Family, Garage, View, Heating.Sources,
                                Heating.System)

# Convert view, heating sources and system values to numeric
model_df[, "View"] <- as.numeric(model_df[, "View"])
model_df[, "Heating.Sources"] <- as.numeric(model_df[, "Heating.Sources"])
model_df[, "Heating.System"] <- as.numeric(model_df[, "Heating.System"])

###############################################################################

###################
#### MLR Model ####
###################
set.seed(1)

# Partition data into 70% training and 30% validating
train.index <- sample(1:nrow(model_df), nrow(model_df) * 0.7)
train.df <- model_df[train.index, ]
valid.df <- model_df[-train.index, ]

# Perform linear regression on the model with zestimate as 
# the outcome variable
model.df.lm <- lm(Zestimate ~ ., data = train.df)

# Examine the statistically significant factors
options(scopen = 999)
summary(model.df.lm)

######################
### Random Forrest ###
######################
rf.model_df <- model_df

# Create new column to categorize home value
rf.model_df$Value.Quartile <- NA

# Extract quartile data from home value
quartile_data <- as.data.frame(quantile(rf.model_df$Zestimate))

# Populate the home value quartile column
for (i in 1:nrow(rf.model_df)) {
  ob = ""
  if (rf.model_df[i, "Zestimate"] > quartile_data[4, ]) {
    ob = ">75%"
  } else if (rf.model_df[i, "Zestimate"] > quartile_data[3, ]) {
    ob = "50-75%"
  } else if (rf.model_df[i, "Zestimate"] > quartile_data[2, ]) {
    ob = "25-50%"
  } else {
    ob = "<25%"
  }
  rf.model_df[i, "Value.Quartile"] <- ob
}

# Convert value quartile format to factor and removes original zestimate
# column
rf.model_df[, "Value.Quartile"] <- 
  as.factor(rf.model_df[, "Value.Quartile"])
rf.model_df <- rf.model_df %>% select(-Zestimate)

# Partition data
set.seed(1)
train.index <- sample(1:nrow(knn.model_df), nrow(knn.model_df) * 0.7)
train.df <- rf.model_df[train.index, ]
valid.df <- rf.model_df[-train.index, ]
# Create the random forest model
model.rf <- randomForest(Value.Quartile ~ ., 
                         data = train.df, ntree = 400, importance = TRUE)

# Store the accuracy of the model in a variable
forest_accuracy = (1 - model.rf$err.rate[[nrow(model.rf$err.rate)]]) * 100
forest_accuracy = format(round(forest_accuracy, 2), nsmall = 2)

# Predict the classification of hoem value in validation set
model.rf.pred <- predict(model.rf, valid.df[, -ncol(valid.df)])
# table(observed = valid.df[, ncol(valid.df)], predicted = model.rf.pred)
