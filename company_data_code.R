library(caTools)
library(party)
library(gmodels)

company_data<-read.csv(file.choose())
str(company_data)

check<-ifelse(company_data$Sales<=9,"Low","High")

str(check)
com_new<-data.frame(company_data,check)
View(com_new)


train_data<-com_new[1:200,]
test_data<-com_new[201:400,]


com_tree = ctree(check ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
                 + Age + Education + Urban + US, data = train_data)
plot(com_tree)

summary(com_tree)



#Prediction
pred_tree<-as.data.frame(predict(com_tree,new=test_data))
pred_test_data <- predict(com_tree,new=test_data)
pred_test_data




plot(pred_test_data)

mean(pred_test_data==test_data$check)



library(gmodels)
CrossTable(test_data$check,pred_test_data)
library(caret)
confusionMatrix(test_data$check,pred_test_data)





############# USING TREE FUNCTION#############
treefunc <- tree(check~.-Sales,data=com_new)
summary(treefunc)
plot(treefunc)
text(treefunc) 


####using train data
treetrain <- tree(check~.-Sales,data=train_data)

summary(treetrain)
plot(treetrain)
text(treetrain)           



pred_tree<-as.data.frame(predict(com_tree,new=test_data))
pred_test_data <- predict(com_tree,new=test_data)

pred_test_data
plot(pred_test_data)

mean(pred_test_data==test_data$check)

CrossTable(test_data$check,pred_test_data)



