library(forecast)
library(fpp)
library(smooth)
install.packages("readxl")
library(readxl)

Airlines <- read_excel(file.choose()) # read the Airlines data
View(Airlines) # Quarterly 4 months 
windows()
plot(Airlines$Passengers,type="o")


X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
# View(X)

colnames(X)<-month.abb # Assigning month names 
# View(X)
AirlinesData<-cbind(Airlines,X)
View(AirlinesData)
colnames(AirlinesData)


AirlinesData["t"]<- 1:96
View(AirlinesData)
AirlinesData["log_Passenger"]<-log(AirlinesData["Passengers"])
AirlinesData["t_square"]<-AirlinesData["t"]*AirlinesData["t"]
attach(AirlinesData)

trainairline<-AirlinesData[1:84,]

tesAirline<-AirlinesData[85:96,]

########################### LINEAR MODEL #############################

linear_model<-lm(Passengers~t,data=trainairline)
summary(linear_model)

linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =tesAirline))
View(linear_pred)
rmse_linear<-sqrt(mean((tesAirline$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear #53.19924

######################### Exponential #################################

expo_model<-lm(log_Passenger~t,data=trainairline)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=tesAirline))
rmse_expo<-sqrt(mean((tesAirline$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo ## 46.05736


######################### Quadratic ####################################

Quad_model<-lm(Passengers~t+t_square,data=trainairline)
summary(Quad_model)

Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=tesAirline))
rmse_Quad<-sqrt(mean((tesAirline$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad #48.05189

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=trainairline)
summary(sea_add_model)

sea_add_pred<-data.frame(predict(sea_add_model,newdata=tesAirline,interval='predict'))
rmse_sea_add<-sqrt(mean((tesAirline$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add  # 132.8198

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=trainairline)
summary(Add_sea_Linear_model)

Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=tesAirline))
rmse_Add_sea_Linear<-sqrt(mean((tesAirline$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 35.34896


######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=trainairline)
summary(Add_sea_Quad_model)

Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=tesAirline))
rmse_Add_sea_Quad<-sqrt(mean((tesAirline$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 26.36082

multi_sea_model<-lm(log_Passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = trainairline)
summary(multi_sea_model)

multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=tesAirline,interval='predict'))
rmse_multi_sea<-sqrt(mean((tesAirline$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 140.0632

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = trainairline)
summary(multi_add_sea_model) 

multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=tesAirline,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((tesAirline$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 10.51917

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

new_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = AirlinesData)
new_model_pred<-data.frame(predict(new_model,newdata=AirlinesData,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)

View(new_model_fin)
Month <- as.data.frame(AirlinesData$Passengers)

Final <- as.data.frame(cbind(Month,airlinedata$Passengers,new_model_fin))
colnames(Final) <-c("Month","Passengers","New_Pred_Value")
plot(Final$Passengers,main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter",
     col.axis="blue",type="o") 

plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Quarter",
     col.axis="Green",type="s")

View(Final)



