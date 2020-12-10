
library(MASS)
library(caret)
install.packages("ggplot2")
library(ggplot2)
library(caTools)
install.packages("randomForest")
library(randomForest)

####Import the data

random_fraud<-read.csv(file.choose())
str(random_fraud)
View(random_fraud)


check<-ifelse(random_fraud$Taxable.Income<=30000,"Risky","Good")
check
View(check)
fraud_data<-data.frame(random_fraud,check)
str(fraud_data)
View(fraud_data)

table(fraud_data$check)

hist(random_fraud$Taxable.Income)



###Splitting of data into train and test
set.seed(123)
sample<-sample.split(fraud_data,SplitRatio = 0.85)
train<-subset(fraud_data,sample==TRUE)
test<-subset(fraud_data,sample==FALSE)

###MODEL BUILDING
model100<-randomForest(check~.,data =train)
model100

pred<-predict(model100,train)
head(pred)
head(train$check)

table(pred,train$check)
CrossTable(train$check,pred,prop.chisq = FALSE)
library(gmodels)


##Model for test data
pred2<-predict(model100,test)
pred2
head(pred2)
table(pred2, test$check) 

CrossTable(test$check,pred2,prop.chisq = FALSE)
library(gmodels)


plot(model100)
legend("topright",colnames(model100$err.rate),col=1:3,cex=0.8,fill=1:3)
varImpPlot(model100)



conf<-table(test$check)
conf



# To check important variables
importance(model100)

varImpPlot(model100)        

