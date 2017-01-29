training_data <- read.table("C:\\Users\\Aastha\\Documents\\yeast_training.txt", sep=" ", header = TRUE)
test_data <- read.table("C:\\Users\\Aastha\\Documents\\yeast_test.txt", sep=" ", header = TRUE)
attach(training_data)
attach(test_data)
require(neuralnet)
nn <- neuralnet(class ~ attribute1 + attribute2 + attribute3 + attribute4 + attribute5 + attribute6 + attribute7 + attribute8, data=training_data, hidden = 3, err.fct="sse", linear.output = TRUE, stepmax=1e6)

plot(nn)
pr <- compute(nn, test_data[, 1:8])
MSE.nn <- sum((pr$net.result - test_data$class)^2)/nrow(test_data)

plot(test_data$class,pr$net.result,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')
