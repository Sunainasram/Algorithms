Aviation <-read.csv(file.choose())
View(Aviation)
str(Aviation)

library(forecast)
plot(Aviation$Sales,type="o")

There is level ,Linear trend ,Additive Seasonality

##Creating 4 dummy variables for Q1,Q2,Q3,Q4,QI
df<-data.frame(Aviation)
df$Q1 <- ifelse(df$Quarter == 'Q1', 1, 0)
df$Q2 <- ifelse(df$Quarter == 'Q2', 1, 0)
df$Q3 <- ifelse(df$Quarter == 'Q3', 1, 0)
df$Q4 <- ifelse(df$Quarter == 'Q4', 1, 0)

install.packages('fastDummies')
library(fastDummies)

dataf <- dummy_cols(df, select_columns = 'Quarter')
datax<-cbind(dataf,Aviation)
View(datax)

###Train and Test Data 
###Data splitting
set.seed(123)
require(caTools)
library(caTools)

sample<-sample.split(datax,SplitRatio = 0.75)
train<-subset(datax,sample==TRUE)
test<-subset(datax,sample==FALSE)

View(datax)
colnames(datax)[2]<-"Sales"
colnames(datax)

datax["t"]<- 1:42


linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Ridership-linear_pred$fit)^2,na.rm = T))
rmse_linear # 209.9256