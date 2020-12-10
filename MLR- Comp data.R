comp_mlr<-read.csv(file.choose())
View(comp_mlr)
comp_mlr<-comp_mlr[-1]
str(comp_mlr)
summary(comp_mlr)

pairs(comp_mlr)

attach(comp_mlr)
plot(comp_mlr)

library(plyr)
library(dplyr)


model <- lm(price ~ speed + hd + ram + screen + ads + trend + cd + multi + premium, data = comp_mlr)
summary(model)

avPlots(model)

install.packages("dplyr")
library(dplyr)


vif(model)
install.packages("corpcor")
library(corpcor)

install.packages("mvinfluence")
library(mvinfluence)

influence.measures(model)
influenceIndexPlot(model)
influencePlot(model)


model3 <- lm(price ~ speed + hd + ram + screen + ads + trend + premium, data = comp_mlr[-c(1441, 1701),])
summary(model3)

avPlots(model3)

plot(model)


