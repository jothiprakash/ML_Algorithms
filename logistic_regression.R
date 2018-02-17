setwd("D:/Backup/Term - 2/Machine Learning/Algorithms")

# Import necessary packages
library(caTools)

# Import the dataset
data <- read.csv("Pima.te.csv")
View(data)
data$X <- NULL

# Data splitting
split <- sample.split(data,
                      SplitRatio = 0.8)
training <- subset(data,
                   split == "TRUE")
testing <- subset(data,
                  split == "FALSE")

# Model
model <- glm(type~.,
             training,
             family = "binomial")
# Null deviance: 309.60  on 248  degrees of freedom
# Residual deviance: 203.47  on 241  degrees of freedom
# AIC: 219.47

res <- predict(model,
               training,
               type = "response")

# Finding the "THRESHOLD"
library(ROCR)

ROCRPred <- prediction(res,
                       training$type)
ROCRPref <- performance(ROCRPred,
                        "tpr",
                        "fpr")
plot(ROCRPref,
     colorize = TRUE,
     print.cutoffs.at = seq(0.1, by = 0.1))

# R & D on Model - Optimizing the model
model <- glm(type~. -age,
             training,
             family = "binomial")
# Null deviance: 309.60  on 248  degrees of freedom
# Residual deviance: 203.53  on 242  degrees of freedom
# AIC: 217.53

model <- glm(type~. -bp,
             training,
             family = "binomial")
# Null deviance: 309.6  on 248  degrees of freedom
# Residual deviance: 203.7  on 242  degrees of freedom
# AIC: 217.7

model <- glm(type~. -npreg,
             training,
             family = "binomial")
# Null deviance: 309.6  on 248  degrees of freedom
# Residual deviance: 211.9  on 242  degrees of freedom
# AIC: 225.9
# the residual deviance and AIC is on rise. So we won't
# remove "npreg"

model <- glm(type~. -skin,
             training,
             family = "binomial")
# Null deviance: 309.60  on 248  degrees of freedom
# Residual deviance: 203.52  on 242  degrees of freedom
# AIC: 217.52

model <- glm(type~. -skin -bp - age,
             training,
             family = "binomial")

# plot(model)
summary(model)

res <- predict(model,
               testing,
               type = "response")
res

cmatrix <- table(ActualValue = testing$type,
                 PredictedValue = res > 0.3)
cmatrix
sum(diag(cmatrix))/sum(cmatrix)
