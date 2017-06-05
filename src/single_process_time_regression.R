#
rm(list=ls())
library(e1071)
#
head <- c("pid", "name", "id", "time-start", "cpu", "mem", "time")
index <- c(1:10)
x.rmse.train <- c()
x.rmse.test <- c()
for(i in c(1:10)){
  index.train <- sample(index, 7)
  index.test <- setdiff(index, index.train)
  #
  train <- data.frame()
  test <- data.frame()
  file <- "data/process10-metric-time.txt"
  process <- as.data.frame(read.table(file, sep="\t", header=FALSE))
  colnames(process) <- head
  train <- rbind(train, process[index.train,])
  test <- rbind(test, process[index.test,])
  # regression time consuming of process
  train.set <- train[, c(5, 6, 7)]
  test.set <- test[, c(5, 6, 7)]
  # Create a linear regression model
  # model <- lm(time ~ ., train.set)
  # svm 
  # svm.tune <- tune(svm, time ~ .,  data = train.set, ranges = list(epsilon = seq(0,1,0.1), cost = c(1:10), gamma = seq(1, 1, 0.1)))
  # model <- svm.tune$best.model
  model <- svm(time ~ ., data = train.set)
  # train acc
  ytrain.pred <- predict(model, train.set)
  error.train <- ytrain.pred - train.set$time
  rmse.train <- sqrt(mean(error.train^2))
  # test acc
  ytest.pred <- predict(model, test.set)
  error.test <- ytest.pred - test.set$time
  rmse.test <- sqrt(mean(error.test^2))
  #
  x.rmse.train <- c(x.rmse.train, rmse.train)
  x.rmse.test <- c(x.rmse.test, rmse.test)
}
# visualize data
par(mfrow=c(2,2)) 
# metric
x.bar <- c(1:length(process$cpu))
y.bar <-range(c(1:max(process$mem, process$cpu)))
# cpu
plot(x.bar, process$cpu, type="b", lwd=1.5, lty=1, col=1, pch=1, xlab = "order of process", ylab = "cpu information")
# time regression
time.pred <- c(ytrain.pred, as.vector(ytest.pred))
plot(x.bar, time.pred, type="b", lwd=1.5, lty=3, col=3, pch=3, xlab = "order of process", ylab = "time regression")
# mem
plot(x.bar, process$mem, type="b", lwd=1.5, lty=2, col=2, pch=2, xlab = "order of process", ylab = "mem information")
# time
time.real <- c(train.set$time, test.set$time)
plot(x.bar, time.real, type="b", lwd=1.5, lty=3, col=3, pch=3, xlab = "order of process", ylab = "time consuming")
lines(x.bar, time.pred, type="b")
#
print(mean(x.rmse.train))
print(mean(rmse.test))
compare.time <- data.frame(time.real, time.pred)
# View(compare.time)


