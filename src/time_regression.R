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
print(rmse.train)
print(rmse.test)
#

