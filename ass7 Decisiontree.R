###decision tree on fraud data
library(caret)
library(C50)
data <- read.csv(file.choose())
View(data)
str(data)
###data partition in training and test data
pd <- createDataPartition(data$Marital.Status,p=0.75,list = F)
training <- data[pd,]
testing <- data[-pd,]
###decision tree model
tree <- C5.0(training$Marital.Status~.,data = training)
tree
plot(tree)
summary(tree)

###predict value
pred <- predict.C5.0(tree,testing)
pred

### missclassification error for train data
a <- table(testing$Marital.Status,pred)
print(a)
sum(diag(a)/sum(a)) #### Missclassification  error is 1-0.6510067=0.3489933

## Decision tree with using rpart
library(rpart)
tree1 <- rpart(Marital.Status~.,data = training)
library(rpart.plot)
rpart.plot(tree1,extra = 1)
