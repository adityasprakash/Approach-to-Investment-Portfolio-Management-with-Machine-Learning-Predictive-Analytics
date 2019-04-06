library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)

all_data <- read.csv("Classification tree.csv")

dim(all_data)

# sampling the data into training and test sets in 75% and 25% respectivley
sample_rows <- sample(60, 45)

train_data <- all_data[sample_rows,]
dim(train_data)

test_data <- all_data[-sample_rows,]
dim(test_data)

  
#plotcp(m)

#Tree_model <- rpart(Categorical.NIFTY.Bank.return ~ Normalized.IIPG.lag + Normalized.Fx.rate.change.lag + Normalized.M3.change.lag + Normalized.FER.change + repo.rate , data = train_data, method = "class")
#Tree_model <- rpart(Categorical.NIFTY.Bank.return ~ ., data = train_data, method = "class", control = rpart.control(cp=0))
Tree_Model <- rpart(Categorical_NIFTYBank_Return ~ ., data = train_data, method = "class", minsplit =5, control = rpart.control(cp=0.2))

Tree_Model

plotcp(Tree_Model)

rpart.plot(Tree_Model)
#Tree_model
#plotcp(Tree_model)
 
p <- predict(Tree_Model, test_data, type = "class")

mean(p == test_data$Categorical_NIFTYBank_Return)


