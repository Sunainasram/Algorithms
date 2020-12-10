data_nn<-read.csv(file.choose())
str(data_nn)
View(data_nn)
data_nn$State <- as.numeric(data_nn$State,c("New York"="0", "California"="1","Florida"="2"))
str(data_nn$State)
data111<-data.frame(data_nn)
View(data111)


##normalizing the data
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
norm_data1<-as.data.frame(lapply(data111[1:3],FUN=normalize))
norm_data2<-as.data.frame(lapply(data111[5],FUN=normalize))
View(norm_data2)

norm_data<-data.frame(norm_data1,norm_data2,data111[4])
View(norm_data)
summary(norm_data$Profit)
View(norm_data)
summary(norm_data)


hist(norm_data$Profit)
cor(norm_data)
pairs(norm_data)

####Data splitting
require(caTools)
library(caTools)
set.seed(123)
sample1<-sample.split(norm_data,SplitRatio = 0.70)
train1<-subset(norm_data,sample1==TRUE)
test1<-subset(norm_data,sample1==FALSE)

###Model Building
library(neuralnet)

model_data<-neuralnet(Profit~Administration+R.D.Spend+State+Marketing.Spend,data = train1,hidden = 3,linear.output = T)
model_data
summary(model_data)

#plot the neural network
plot(model_data)


###prediction using Neural network
model_comp<-compute(model_data, test1[,c(1:4)])
predict_testNN = (model_comp$net.result)
cor(predict_testNN,test1$Profit)
plot(predict_testNN,test1$Profit)

