########To import the data
#read the data
ff<- read.csv(file.choose())
require(caTools)

##normalize the data
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
ff$temp = normalize(ff$temp)

ff$RH   = normalize(ff$RH)
ff$wind = normalize(ff$wind)
ff$rain = normalize(ff$rain)

attach(ff)


###random split on data - using Catool function
data1= sample.split(ff,SplitRatio = 0.3)

###Train data
train =subset(ff,data1==TRUE)

####Test data
test =subset(ff,data1==FALSE)


###visualisation
hist(ff$DMC)
hist(ff$FFMC)
hist(ff$temp)

#########model building############
library(kernlab)
model1<-ksvm(size_category~temp+rain+wind+RH,data= train,kernel = "vanilladot")

####prediction######

prediction <- predict(model1, test)


table(prediction,test$size_category)

###to compare###
agreement <- prediction == test$size_category
table(agreement)

###Accuracy of the model###
mean(prediction==test$size_category)

###################################################################################

##to improve the model performance by using different kerner functions

model2<-ksvm(size_category~temp+rain+wind+RH,data= train,kernel = "rbfdot")
prediction2 <- predict(model2, test)
table(prediction2,test$size_category)

agreement2<-prediction2==test$size_category
table(agreement2)

mean(prediction2==test$size_category)

#####

model3<-ksvm(size_category~temp+rain+wind+RH,data= train,kernel = "poydot")
prediction3 <- predict(model3, test)
table(prediction3,test$size_category)
agreement3<-prediction3==test$size_category
table(agreement3)

mean(prediction3==test$size_category)

####################
model4<-ksvm(size_category~temp+rain+wind+RH,data= train,kernel = "laplacedot")
prediction4 <- predict(model4, test)
table(prediction4,test$size_category)
agreement4<-prediction3==test$size_category
table(agreement4)

mean(prediction4==test$size_category)





