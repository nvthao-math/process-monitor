# args <- commandArgs(trailingOnly = TRUE)
args <- commandArgs(TRUE)
x <- as.numeric(args[1])
y <- as.numeric(args[2])
sum <- x + y
print(sum)
