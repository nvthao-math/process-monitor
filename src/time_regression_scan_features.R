rm(list=ls())
library(e1071)
library(jsonlite)
#
head <- c("pid", "name", "id", "time-start", "cpu", "mem", "time")
index <- c(1:10)
index.train <- sample(index, 7)
index.test <- setdiff(index, index.train)
#
train <- data.frame()
test <- data.frame()
files <- list.files("/home/cpu10869-local/R/process-monitor/data", full.names=TRUE)
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
file.regression <- "/home/cpu10869-local/R/process-monitor/result/scan_feature.txt"
epsilon = seq(0.42,1,0.01)
cost = seq(1, 10, 0.1)
gamma = seq(0, 1, 0.01)
for(ep_i in epsilon){
  for(c_i in cost){
    for(g_i in gamma){
      model <- svm(time ~ ., data = train.set, cost = c_i, gamma = g_i, epsilon = ep_i)
      # train acc
      ytrain.pred <- predict(model, train.set)
      error.train <- ytrain.pred - train.set$time
      rmse.train <- sqrt(mean(error.train^2))
      # test acc
      ytest.pred <- predict(model, test.set)
      error.test <- ytest.pred - test.set$time
      rmse.test <- sqrt(mean(error.test^2))
      # save data
      df <- data.frame(ep_i, c_i, g_i, rmse.test, rmse.train)
      line <- toJSON(df)
      print(line)
      write(line, file=file.regression, append=TRUE)
    }
  }
}


