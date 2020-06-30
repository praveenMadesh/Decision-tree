##decision tree on company data

library(caret)
library(C50)

data <- read.csv(file.choose())### company data
View(data)
str(data)

###partition data inti training and testing data
pd <- createDataPartition(data$ShelveLoc,p=0.75,list = F)
training <- data[pd,]
testing <- data[-pd,]

###decision tree model
tree <- C5.0(training$ShelveLoc~.,data = training)
tree
plot(tree)
summary(tree)

###predicted values
pred <- predict.C5.0(tree,testing)
pred

## missclassification error for train data
a <- table(testing$ShelveLoc,pred)
print(a)
sum(diag(a)/sum(a))### missclassification or error 1-0.5353535 = 0.4646465 

###Decision tree iwth using "rpart"
library(rpart)
tree1 <- rpart(ShelveLoc~.,training)
tree1
install.packages("rpart.plot") 
library(rpart.plot)
rpart.plot(tree1,extra = 2)
