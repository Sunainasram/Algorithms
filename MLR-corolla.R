corolla_mlr<-read.csv(file.choose())
corolla_mlr<-corolla_mlr[-1]
View(corolla_mlr)

Corolla<-corolla_mlr[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(Corolla)

str(Corolla)

attach(Corolla)
plot(Corolla)
pairs(Corolla)

plot(Price,Age_08_04)
plot(Price,Weight)
plot(Price,Quarterly_Tax)
plot(Price,Gears)
plot(Price,Doors)
plot(Price,cc)
plot(Price,KM)
plot(Price,HP)


cor(Corolla)
install.packages("corpcor")
library(corpcor)

cor2pcor(cor(Corolla))

model_cor<-lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = Corolla)
model_cor
summary(model_cor)

model_cc<-lm(Price~cc,data = Corolla)
summary(model_cc)



model_door<-lm(Price~Doors,data = Corolla)
summary(model_door)

model_ccdoor<-lm(Price~cc+Doors,data = Corolla)
summary(model_ccdoor)

influence.measures(model_ccdoor)
install.packages("mvinfluence")
library(mvinfluence)
influenceIndexPlot(model_ccdoor)
influencePlot(model_ccdoor)

model_fin <- lm(Price ~ ., data = Corolla[-c(81),])
summary(model_fin)

vif(model_fin)
avPlots(model_fin)

finalmodel <- lm(Price ~ Age_08_04 + KM + HP + cc + Gears + Quarterly_Tax + Weight, data = Corolla[-c(81),])
summary(finalmodel)
plot(finalmodel)
