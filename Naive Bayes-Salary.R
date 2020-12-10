install.packages("e1071")
library(e1071)

train<-read.csv(file.choose())

test<-read.csv(file.choose())

pairs(train[,1:14])

str(train)
str(test)

is.data.frame(train)

is.factor(train$race)
is.factor(train$sex)
is.factor(train$Salary)
is.factor(train$occupation)
is.factor(train$education)
is.factor(train$capitalgain)

##dim of the data
prop.table(table(train$Salary))*100
prop.table(table(test$Salary))*100



##build a model
?naiveBayes
model<-naiveBayes(train$Salary~.,data = train[-14])
model
class(model)

##test a model

pred<-predict(model,test)

pred

#confusion matrix
cm<-table(pred,test$Salary)
cm

#crosstable
library(gmodels)
CrossTable(pred,test$Salary)

##Inference:
#10550 entries are correctly classified as salary<=50k
#1789 entries are correctly classified as salary>=50k

##accuracy of the model
mean(pred==test$Salary)


