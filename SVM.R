training_data <- read.table("C:\\Users\\Aastha\\Documents\\yeast_training.txt", sep=" ", header = TRUE)
test_data <- read.table("C:\\Users\\Aastha\\Documents\\yeast_test.txt", sep=" ", header = TRUE)
attach(training_data)
attach(test_data)

install.packages("e1071")

set.seed(1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10),rep(1,10))

x[y==1,] =x[y==1,]+1
plot(x,col=(3-y))
dat=data.frame(x=x,y=as.factor(y))
library(e1071)
svmfit =svm(y~., data=dat,kernel="linear", cost=10, scale = FALSE)
dim(svmfit)

plot(svmfit,dat)
svmfit$index
summary(svmfit)
bestmodel = tune.out$best.model
summary(bestmodel)
xtest =matrix(rnorm(40), ncol=2)
ytest=sample(c(-1,1),20,rep=TRUE)
xtest[ytest==1]=xtest[ytest==1,]+1
testdat =data.frame(x=xtest, y=as.factor(ytest))
testdat
ypred =predict(bestmodel, testdat)
table(predict=ypred, truth=testdat$y)
set.seed(1)
x=matrix(rnorm(400),ncol=2)
x[1:100,] =x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat= data.frame(x=x,y=as.factor(y))
plot(x,col=y)
train =sample(200,100)
svmfit =svm(y~.,data=dat[train,],kernel ="radial", gamma=1, cost=1)
plot(svmfit,dat[train,])
summary(svmfit)
svmfit =svm(y~.,data=dat[train,],kernel ="radial", gamma=1, cost=10000)
tune.out =tune(svm,y~.,data=dat,kernel="radial",ranges = list(cost =c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)