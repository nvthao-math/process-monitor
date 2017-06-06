#
rm(list=ls())
library(e1071)
#
head <- c("pid", "name", "id", "time-start", "cpu", "mem", "time")
index <- c(1:10)
# regression time consuming of process
model.path <- '/home/cpu10869-local/sandbox/process-monitor/regression_model/svm_regression_model_best.rds'
model.flag.path <- "/home/cpu10869-local/sandbox/process-monitor/regression_model/model_flag.log"
model.best <- list()
# set flag of training status
model.flag <- "FALSE"
write(model.flag, file=model.flag.path, append=FALSE)
rmse.test.best <- .Machine$double.xmax
rmse.train.best <- .Machine$double.xmax
test.s <- data.frame()
ytest.s <- c()
for(i in c(1:1000)){
  index.train <- sample(index, 7)
  index.test <- setdiff(index, index.train)
  #
  train <- data.frame()
  test <- data.frame()
  files <- list.files("/home/cpu10869-local/R/process-monitor/data", full.names=TRUE)
  for(file in files){
    # print(file)
    process <- as.data.frame(read.table(file, sep="\t", header=FALSE))
    colnames(process) <- head
    train <- rbind(train, process[index.train,])
    test <- rbind(test, process[index.test,])
  }
  #
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
  if(rmse.test < rmse.test.best & rmse.train < rmse.train.best){
    rmse.test.best <- rmse.test
    rmse.train.best <- rmse.train
    model.best <- model
    test.s <- test.set
    ytest.s <- ytest.pred
  }
}
# update flag status and save model
model.flag <- "TRUE"
write(model.flag, file=model.flag.path, append=FALSE)
write(paste("rmse.test.best: ", rmse.test.best), file=model.flag.path, append=TRUE)
write(paste("rmse.train.best: ", rmse.train.best), file=model.flag.path, append=TRUE)
saveRDS(model.best, file = model.path)
