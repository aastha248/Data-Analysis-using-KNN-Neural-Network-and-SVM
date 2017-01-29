training_data <- read.table("C:\\Users\\Aastha\\Documents\\yeast_training.txt", sep=" ", header = TRUE)
test_data <- read.table("C:\\Users\\Aastha\\Documents\\yeast_test.txt", sep=" ", header = TRUE)
attach(training_data)
attach(test_data)
summary(training_data)

normalize <- function(x){
  return((x - min(x))/(max(x)-min(x)))
}

new_training_data <- as.data.frame(lapply(training_data[,c(1,2,3,4,5,6,7,8)], normalize))
new_test_data <- as.data.frame(lapply(test_data[,c(1,2,3,4,5,6,7,8)], normalize))
training_data_target <- training_data[, c(9)]
test_data_target <- test_data[, c(9)]
require(class)
nrow(training_data_target)
model <- knn(train = new_training_data, test = new_test_data, cl = training_data_target, k = 31)
model
plot(model)
table(model, test_data_target)
error_rate <- mean(model != test_data_target)

accuracy <- rep(0, 50)
k <- 1:50
for(x in k){
  prediction <- knn(train = new_training_data, test = new_test_data, cl = training_data_target, k = x)
  accuracy[x] <- 1 - mean(prediction != test_data_target)
}
plot(k, accuracy, type = 'b')
