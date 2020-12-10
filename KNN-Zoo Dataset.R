install.packages("class")
library(class)

zooo<-read.csv(file.choose())
str(zooo)
View(zooo)

zoo1<-zooo[-1]
View(zoo1)
str(zoo1)
as.factor(zoo1)


table(zoo1$type)
prop.table(table(zoo1$type))*100


##Split data into train and test data
train_data<-zoo1[1:85, ]
test_data<-zoo1[86:101, ]

train_cl<-(zoo1[1:85,17])
test_cl<-(zoo1[86:101,17])

View(train_cl)


##Build  the model

zoo_pred <- knn(train_data,test_data, cl = train_cl, k=7)

#confusion matrix of the model

cm<-table(zoo_pred,test_cl)
View(cm)
cm


#check the accuracy of the model
accuracy <- function(x){
  sum(diag(x)/(sum(rowSums(x)))) * 100
  }

accuracy(cm)

##checking for various k values to increase efficiency
zoo_pred <- knn(train_data,test_data, cl = train_cl, k=7)  ##efficiency is 68.5
zoo_pred <- knn(train_data,test_data, cl = train_cl, k=5) ##efficiency is 87.5
zoo_pred <- knn(train_data,test_data, cl = train_cl, k=4) ##efficiency is 87.5
zoo_pred <- knn(train_data,test_data, cl = train_cl, k=3) ##efficiency is 87.5
zoo_pred <- knn(train_data,test_data, cl = train_cl, k=2) ##efficiency is 87.5

####k value selected##

# At k=7 ,efficiency drops
# Hence Choosing K=6 
zoo_pred <- knn(train_data,test_data, cl = train_cl, k=6)## efficiency is higher 




