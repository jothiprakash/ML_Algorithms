setwd("D:/Backup/Term - 2/Machine Learning/Algorithms/")

diabet <- read.csv("pima-indians-diabetes.csv")

names(diabet)[9] <- "is.diabetic"
diabet$is.diabetic <- as.factor(diabet$is.diabetic)

set.seed(3)
library(caTools)

split <- sample.split(diabet,
                      SplitRatio = 0.7)
diabet_train <- subset(diabet,
                       split == "TRUE")
diabet_test <- subset(diabet,
                      split == "FALSE")

# Before we create random forest, let's find out the best "mtry"
# value using following commands
# Optimised value of "mtry"
library(randomForest)
bestmtry <- tuneRF(diabet_train,
                   diabet_train$is.diabetic,
                   stepFactor = 1.2,
                   improve = 0.01,
                   trace = T,
                   plot = T)

# Random Forest
diabet_forest <- randomForest(is.diabetic~.,
                              data = diabet_train)
diabet_forest

# Gives GINI index (Priority of variables)
diabet_forest$importance

# Let's see what all variables are most important for our model.
varImpPlot(diabet_forest)

# Prediction
pred <- predict(diabet_forest,
                diabet_test,
                type = "class")
pred

library(caret)
confusionMatrix(table(pred,
                      diabet_test$is.diabetic))
