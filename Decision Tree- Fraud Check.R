install.packages('C50')
library(caTools)
library(party)
library(gmodels)
library(C50)
library(tree)
library(gmodels)
library(knitr)
library(png)
install.packages("tree")

FraudCheck <- read.csv(file.choose())
hist(FraudCheck$Taxable.Income)

Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
FC = data.frame(FraudCheck,Risky_Good)

#Splitting data into train and test
FC_train <- FC[1:300,]

View(FC_train)
FC_test <- FC[301:600,]

View(FC_test)

###Using Party Function 
library(party)
#png(file = "decision_tree.png")
opall_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FC)
summary(opall_tree)
library(gmodels)
plot(opall_tree)
# From the above tree, It looks like the data has 20 % of Risky patients and 80 % good patients


# using the training Data 

op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree)

plot(op_tree)

pred_tree <- as.data.frame(predict(op_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=FC_test)


mean(pred_test_df==FC_test$Risky_Good) # Accuracy = 82 %
CrossTable(FC_test$Risky_Good,pred_test_df)


op_tree1 = C50(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree1)
plot(op_tree1)

pred_tree1 <- as.data.frame(predict(op_tree1,newdata=FC_test))
pred_tree1["final"] <- NULL

pred_test_df1 <- predict(op_tree1,newdata=FC_test)
mean(pred_test_df1==FC_test$Risky_Good) # Accuracy = 82 %
CrossTable(FC_test$Risky_Good,pred_test_df1)


CrossTable(FC_test$Risky_Good,pred_test_df1,prop.chisq = FALSE)
library(caret)
confusionMatrix(FC_test$Risky_Good,pred_test_df1)
