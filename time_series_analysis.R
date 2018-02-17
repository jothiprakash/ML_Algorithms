# ARIMA

data("AirPassengers")
class(AirPassengers)

start(AirPassengers)
# This gives the start of the time series

end(AirPassengers)
# This give the end of the time series

frequency(AirPassengers)
# The cycle of this time series is 12 months in a year

summary(AirPassengers)
# The number of passengers are distributed across the spectrum

plot(AirPassengers)
# This will plot the time series

abline(reg = lm(AirPassengers ~ time(AirPassengers)))
# This will fit in a line

cycle(AirPassengers)
# This will print the cycle across years

# Start from here

plot(aggregate(AirPassengers,
               FUN = mean))
# This will aggregate the cycles and display an year on year trend.

boxplot(AirPassengers ~ cycle(AirPassengers))
# Boxplot across months will give us a sense of seasonal effect.



#####################################################################
# Introduction to ARMA time series modeling
# It is commonly used and AR stands for Auto Regression
# MA stands for Moving Average

# We know that we have to address two issues before we test
# stationary series.
# One, we need to remove unequal variances. We do this by taking
# log of the series.
# Two, We need to address the trend component. We do this by taking
# difference of
plot(diff(log(AirPassengers)))
# We see that the series is stationary enough to do time series modeling
# AUGMENTED DICKEY FULLER TEST


acf(AirPassengers)

acf(diff(log(AirPassengers))) 
# Auto Correlation Function
# Determines the value of q

pacf(diff(log(AirPassengers)))
# Partial Auto Correlation Function
# Determines the value of p

plot(diff(log(AirPassengers)))

# Let's fit an ARIMA model and predict the future 10 years
# c(p, d, q)
fit <- arima(x = log(AirPassengers),
             order = c(0, 1, 1),
             seasonal = list(order = c(0, 1, 1),
                             period = 12))

pred <- predict(fit,
                n.ahead = 10 * 12)

pred1 <- 2.718 ^ pred$pred
# Changing log value to normal one by using e value.

ts.plot(AirPassengers,
        2.718 ^ pred$pred,
        log = 'y',
        lty = c(1, 3))


# Testing our model
datawide <- ts(AirPassengers,
               frequency = 12,
               start = c(1949, 1),
               end = c(1959, 12))

fit <- arima(x = log(datawide),
             order = c(0, 1, 1),
             seasonal = list(order = c(0, 1, 1),
                             period = 12))

pred <- predict(fit,
                n.ahead = 10 * 12)

pred1 <- 2.718 ^ pred$pred

data1 <- head(pred1, 12)

predicted_1960 <- round(x = data1,
                        digits = 0)

original_1960 <- tail(AirPassengers, 12)
