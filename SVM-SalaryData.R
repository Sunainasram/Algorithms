train_sal<-read.csv(file.choose())
test_sal<-read.csv(file.choose())

str(train_sal)
class(train_sal)

train_sal$educationno <- as.factor(train_sal$educationno)

test_sal$educationno <- as.factor(test_sal$educationno)
str(train_sal)
str(test_sal)

###model building
library(kernlab)
library(caret)

library(plyr)

##model building
model1_sal<-ksvm(train_sal$Salary~.,data=train_sal,kernel="vanilladot")

###prediction
prediction_sal<-predict(model1_sal,test_sal)

table(prediction_sal,test_sal$Salary)

###comparison
agreement_sal<-prediction_sal==test_sal$Salary
table(agreement_sal)
mean(prediction_sal==test_sal$Salary)

####### try different kernel functions to increase the accuracy
model2_sal<-ksvm(train_sal$Salary~.,data=train_sal,kernel="rbfdot")

###prediction
prediction_sal2<-predict(model2_sal,test_sal)

table(prediction_sal2,test_sal$Salary)

###comparison
agreement_sal2<-prediction_sal2==test_sal$Salary
table(agreement_sal2)
mean(prediction_sal2==test_sal$Salary)

############
model3_sal<-ksvm(train_sal$Salary~.,data=train_sal,kernel="polydot")

###prediction
prediction_sal3<-predict(model3_sal,test_sal)

table(prediction_sal3,test_sal$Salary)

###comparison
agreement_sal3<-prediction_sal3==test_sal$Salary
table(agreement_sal3)
mean(prediction_sal3==test_sal$Salary)*100


#####visualization
hist(train_sal$age)
plot(train_sal$Salary)
