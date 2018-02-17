setwd("D:/Backup/Term - 2/Machine Learning/Algorithms/")

require(xgboost)

# Loading the dataset
data("agaricus.train",
     package = "xgboost")
data("agaricus.test",
     package = "xgboost")
train <- agaricus.train
test <- agaricus.test

# Understanding the dataset
str(train)
dim(train$data)
dim(test$data)

# As seen below, the data are stored in a dgCMatrix 
# which is a sparse matrix and label vector is a numeric vector ({0,1}):
class(train$data)[1]
class(train$label)

# Basic training using XGBoost
# Sparse Matrix
bstSparse <- xgboost(data = train$data,
                     label = train$label,
                     max.depth = 2,
                     eta = 1,
                     nthread = 2,
                     nrounds = 2,
                     objective = "binary:logistic")

# Parameter variations
# Dense Matrix
bstDense <- xgboost(data = as.matrix(train$data),
                    label = train$label,
                    max.depth = 2,
                    eta = 1,
                    nthread = 2,
                    nrounds = 2,
                    objective = "binary:logistic")

# XGBoost offers a way to group them in a xgb.DMatrix.
# You can even add meta data to it.
dtrain <- xgb.DMatrix(train$data,
                      label = train$label)
bstDMatrix <- xgboost(data = dtrain,
                      max.depth = 2,
                      eta = 1,
                      nthread = 2,
                      nrounds = 2,
                      objective = "binary:logistic")

# XGBoost has several features to help you to view how the 
# learning progress internally. The purpose is to help you to 
# set the best parameters, which is the key of your model quality.

# One of the simplest way to see the training progress is to set the
# verbose option

# verbose = 0
bst <- xgboost(data = dtrain,
               max.depth = 2,
               eta = 1,
               nthread = 2,
               nrounds = 2,
               objective = "binary:logistic",
               verbose = 0)

# verbose = 1, print evaluation metric
bst <- xgboost(data = dtrain,
               max.depth = 2,
               eta = 1,
               nthread = 2,
               nrounds = 2,
               objective = "binary:logistic",
               verbose = 1)

# verbose = 2, also print information about the tree.
bst <- xgboost(data = dtrain,
               max.depth = 2,
               eta = 1,
               nthread = 2,
               nrounds = 2,
               objective = "binary:logistic",
               verbose = 2)

# Basic prediction using XGBoost
# Perform the prediction
pred <- predict(bst,
                test$data)
head(pred)

# Transform the regression to a binary classification
prediction <- as.numeric(pred > 0.5)
head(prediction)

# Measuring model performance
# To compute the model performance, we will compute a simple metric,
# the average error.
err <- mean(prediction != test$label)
err

# Advanced features
# Dataset preparation
dtrain <- xgb.DMatrix(data = train$data,
                      label = train$label)
dtest <- xgb.DMatrix(data = test$data,
                     label = test$label)

# Measuring learning progress with xgb.train
watchlist <- list(train = dtrain, 
                  test = dtest)
bst <- xgb.train(data = dtrain,
                 max.depth = 2,
                 eta = 1,
                 nthread = 2,
                 nrounds = 2,
                 watchlist = watchlist,
                 objective = "binary:logistic")

# To use multiple evaluation metrics
bst <- xgb.train(data = dtrain,
                 max.depth = 2,
                 eta = 1,
                 nthread = 2,
                 nrounds = 2,
                 watchlist = watchlist,
                 eval.metric = "error",
                 eval.metric = "logloss",
                 objective = "binary:logistic")

# Linear boosting
bst <- xgb.train(data = dtrain,
                 max.depth = 2,
                 booster = "gblinear",
                 nthread = 2,
                 nrounds = 2,
                 watchlist = watchlist,
                 eval.metric = "error",
                 eval.metric = "logloss",
                 objective = "binary:logistic")

# Manipulating xgb.DMatrix
# Save/Load
# Like saving models, xgb.DMatrix object (which groups both dataset 
# and outcome) can also be saved using xgb.DMatrix.save function.
xgb.DMatrix.save(dmatrix = dtrain,
                 fname = "dtrain.buffer")

# To load it in, simply call xgb.DMatrix
dtrain2 <- xgb.DMatrix("dtrain.buffer")

bst <- xgb.train(
  data = dtrain2,
  max.depth = 2,
  eta = 1,
  nthread = 2,
  nround = 2,
  watchlist = watchlist,
  objective = "binary:logistic"
)

# Information extraction
# Information can be extracted from xgb.DMatrix using getInfo()
# function. Here after we will extract label data.
label <- getinfo(dtest, "label")
pred <- predict(bst,
                dtest)
err <- as.numeric(sum(as.integer(pred > 0.5) != label))/length(label)
print(paste("test-error = ", err))

# View feature importance/influence from the learn't model
importance_matrix <- xgb.importance(model = bst)
xgb.plot.importance(importance_matrix = importance_matrix)

# View the trees from a model
xgb.dump(model = bst,
         with_stats = T)

# We can plot the trees
xgb.plot.tree(model = bst)

# Save and load models
xgb.save(model = bst,
         fname = "xgboost.model")

# load binary model to R
bst2 <- xgb.load("xgboost.model")
pred2 <- predict(bst2, test$data)

# And now the test
print(paste("sum(abs(pred2-pred))=", sum(abs(pred2 - pred))))


# R & D Section
# library(caTools)
# split <- sample.split(Y = iris$Species,
#                       SplitRatio = 0.9)
# train <- subset(x = iris,
#                 split == "TRUE")
# test <- subset(x = iris,
#                split == "FALSE")
# 
# dtrain <- xgb.DMatrix(data = train)
# 
# bst <- xgb.train(data = train,
#                  nrounds = 5,
#                  nthread = 2,
#                  max.depth = 2,
#                  objective = "binary:logistic")