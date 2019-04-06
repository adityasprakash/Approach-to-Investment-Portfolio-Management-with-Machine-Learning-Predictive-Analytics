# KNN
#--------
library(class)

all_data <- read.csv("normalized_data_all.csv")
dim(all_data)

# sampling the data into training and test sets in 75% and 25% respectivley
sample_rows <- sample(60, 45)

train_data <- all_data[sample_rows,]
dim(train_data)

test_data <- all_data[-sample_rows,]
dim(test_data)

# create model for KNNc alssificationusing K=1 and 5
KNN_model <- knn(train_data[-1:-2], test_data[-1:-2], train_data$Categorical.NIFTY.Bank.return, k=5)
# label the actual retruns observed
actual_returns <- test_data$Categorical.NIFTY.Bank.return

# confusion matrix
table(KNN_model,actual_returns)

# Accuracy
mean(KNN_model==actual_returns)

#------------------------------------
