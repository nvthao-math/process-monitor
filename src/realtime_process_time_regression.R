#
args <- commandArgs(TRUE)
# rm(list=ls())
library(e1071)
model.path <- '/home/cpu10869-local/sandbox/process-monitor/regression_model/svm_regression_model_best.rds'
head <- c("id", "cpu", "mem")
input <- data.frame(as.numeric(args[1]), as.numeric(args[2]), as.numeric(args[3]))
colnames(input) <- head
model <- readRDS(model.path)
y.pred <- predict(model, input)
print(y.pred)

