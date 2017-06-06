#
rm(list=ls())
library(e1071)
#
head <- c("pid", "name", "id", "time-start", "cpu", "mem", "time")
index <- c(1:10)
index.train <- sample(index, 7)
index.test <- setdiff(index, index.train)
#
train <- data.frame()
test <- data.frame()
files <- list.files("data", full.names=TRUE)
for(file in files){
  print(file)
  process <- as.data.frame(read.table(file, sep="\t", header=FALSE))
  colnames(process) <- head
  train <- rbind(train, process[index.train,])
  test <- rbind(test, process[index.test,])
}
# regression time consuming of process
train.set <- train[, c(3, 5, 6, 7)]
test.set <- test[, c(3, 5, 6, 7)]
process <- rbind(train.set, test.set)
# Create a linear regression model
model <- svm(time ~ ., data = train.set, cost = 6.1, gamma = 1, epsilon = 0.01)
# train acc
ytrain.pred <- predict(model, train.set)
error.train <- ytrain.pred - train.set$time
rmse.train <- sqrt(mean(error.train^2))
# test acc
ytest.pred <- predict(model, test.set)
error.test <- ytest.pred - test.set$time
rmse.test <- sqrt(mean(error.test^2))
#
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
compare.time <- data.frame(time.real, time.pred)
# View(compare.time)
print(rmse.train)
print(rmse.test)
#

