setwd("D:/Backup/Term - 2/Machine Learning/Algorithms/")

# Import necessary libraries
library(MASS)
data("Boston")

# We will split the data into training and test datasets
set.seed(2)
library(caTools) # sample.split function is present in this package

split <- sample.split(Boston$medv,
                      SplitRatio = 0.7)
# We divided the data with the ratio of 70, 30
split

training_data <- subset(Boston,
                        split == "TRUE")
test_data <- subset(Boston,
                    split == "FALSE")

# Let's find the relation among all the variables through scatteplot
library(lattice)
splom(~Boston[c(1:6, 14)],
      groups = NULL,
      data = Boston,
      axis.line.tck = 0,
      axis.text.alpha = 0)
splom(~Boston[c(7:14)],
      groups = NULL,
      data = Boston,
      axis.line.tck = 0,
      axis.text.alpha = 0)
# The plot shows positive linear trend between rm(average no of rooms)
# and medv(value of home)
# No relevant relationship between indus(proportion on non-retail business)
# and medv
# Studying rm and medv
plot(Boston$rm, Boston$medv)
abline(lm(Boston$medv ~ Boston$rm), col = "red")

# Correlation analysis
cr <- cor(Boston)
library(corrplot)
plot(Boston$crim,
     Boston$medv,
     cex = 0.5,
     xlab = "Crime Rate",
     ylab = "Price")
corrplot(cr,
         type = "lower")
corrplot(cr,
         method = "number")

# Variance Inflation Factor(VIF)
library(car)
model <- lm(medv~.,
            data = training_data)
vif(model)

# Let's find the equation representing the best fit line
summary(model)
plot(model)

# Now let's build a model with the help of training set using the 
# code below, here we will using all variables excluding tax
model <- lm(medv ~ crim +
              zn +
              chas +
              nox +
              rm +
              dis +
              rad +
              ptratio +
              black +
              lstat,
            data = training_data)
summary(model)

# Now we can use our model to predict the output of testing dataset
# We can use the following code for predicting the output
pred <- predict(model, test_data)

# For comparing these values we can use plots
# Here we plot a line graph where green line represents the actual price
# and blue line represents the predicted value
plot(test_data$medv, 
     type = "l",
     lty = 1.8,
     col = "green")
lines(pred,
      type = "l",
      col = "blue")
