forestfire_nn<-read.csv(file.choose())
View(forestfire_nn)
str(forestfire_nn)
forestfire_nn$month<-as.numeric(forestfire_nn$month,c("mar"="3","oct"="10"))
forestfire_nn$day<-as.numeric(forestfire_nn$day,c("dayfri"="5","daymon"="1"))
forestfire_nn$size_category<-as.numeric(forestfire_nn$size_category,c("small"="2","large"="1"))
View(forestfire_nn$size_category)

View(forestfire_nn$month)
View(forestfire_nn$day)
View(forestfire_nn$size_category)
##normalize the data
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forestfire_nn[3:11]=normalize(forestfire_nn[3:11])
View(forestfire_nn[3:11])

data_forest<-data.frame(forestfire_nn[3:11],forestfire_nn[1:2],forestfire_nn[31])
View(data_forest)
str(data_forest)


###Data Splitting
require(caTools)
library(caTools)
set.seed(123)
sample2<-sample.split(data_forest,SplitRatio = 0.7)
train_ff<-subset(data_forest,sample2==TRUE)
test_ff<-subset(data_forest,sample2==FALSE)

###Model Building
library(neuralnet)
model_fire<-neuralnet(area~FFMC+DMC+DC+ISI+temp+RH+wind+rain+size_category,data = train_ff,hidden = 3,linear.output = T)
summary(model_fire)

#plot the neural network
plot(model_fire)


###prediction using Neural network
model_compute<-compute(model_fire, test_ff[,c(1:8,10:12)])

predict_testNN1 =(model_compute$net.result)
cor(predict_testNN1,test_ff$area)
plot(predict_testNN1,test_ff$area)




