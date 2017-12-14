library(mlbench)
library(nnet)
data("BostonHousing")

summary(BostonHousing)

plot(BostonHousing$medv)

lm.fit <- lm(medv ~ ., data = BostonHousing)
lm.predict <- predict(lm.fit)

mean((lm.predict - BostonHousing$medv)^2)

# plot(BostonHousing$medv, lm.predict, main = "Linear Regression Predictions vs Actual", xlab = "Actual")

nnet.fit <- nnet(medv/50 ~ ., data = BostonHousing, size = 10)
nnet.predict <- predict(nnet.fit) * 50

mean((nnet.predict - BostonHousing$medv)^2)

plot(BostonHousing$medv, nnet.predict,
     main="Neural Network Predictions vs Actual",
     xlab="Actual")
