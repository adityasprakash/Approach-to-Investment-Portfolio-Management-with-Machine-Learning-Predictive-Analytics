library(randomForest)

# loading the data
forest_data <- read.csv("Random Forest.csv")
summary(forest_data)

# creating the random forest model
Forest_model <- randomForest(Categorical_NIFTYBank_Return ~ ., data = forest_data, method = "class", ntree = 500)

# confusion matrix and OOB estimate of the model
Forest_model

