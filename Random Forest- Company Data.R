comp_data<-read.csv(file.choose())
str(comp_data)

sales_data<-ifelse(comp_data$Sales<9, "No", "Yes") 
comp_data_rf<-data.frame(sales_data,comp_data)
View(comp_data_rf)
str(comp_data_rf)

table(comp_data_rf$sales_data)


###Data splitting
set.seed(123)
require(caTools)
library(caTools)

sample<-sample.split(comp_data_rf,SplitRatio = 0.75)
train<-subset(comp_data_rf,sample==TRUE)
test<-subset(comp_data_rf,sample==FALSE)

library(randomForest)

##Model Building
model_rf<-randomForest(sales_data~., data = train)
model_rf
summary(model_rf)

#Prediction
model_pred<-predict(model_rf,train)
model_pred
head(model_pred)

table(model_pred,train$sales_data)

##on test data
model_pred1<-predict(model_rf,test)
model_pred1                  
head(model_pred1)

table(model_pred1,test$sales_data)


##Accuracy of the model on train data
library(gmodels)
CrossTable(train$sales_data,model_pred,prop.chisq = FALSE)


##Accuracy on test data
CrossTable(test$sales_data,model_pred1,prop.chisq = FALSE)
library(gmodels)



plot(model_rf)
legend("topright",colnames(model_rf$err.rate),col=1:3,cex=0.8,fill=1:3)
varImpPlot(model_rf)

conf<-table(test$sales_data)
conf

