setwd("D:/Backup/Term - 2/Machine Learning/Algorithms/")

# Import the dataset
diabet <- read.csv("pima-indians-diabetes.csv")

# Split data
# id <- sample(2,
#              nrow(diabet),
#              prob = (0.7,0.3))

library(rpart)
model <- rpart(Class.variable ~ .,
               data = diabet)

model

plot(model,
     margin = 0.1)

text(model,
     use.n = T,
     pretty = T,
     cex = 0.8)

# Prediction of test dataset
pred <- predict(model,
                newdata = diabet,
                type = "class")
pred

cmatrix <- table(pred > 0.5,
                 diabet$Class.variable)
cmatrix

library(caret)
confusionMatrix(cmatrix)

accuracy <- sum(diag(cmatrix))/sum(cmatrix)
accuracy
