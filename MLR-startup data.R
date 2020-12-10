start_mlr<-read.csv(file.choose())
View(start_mlr)

summary(start_mlr)
install.packages("dplyr")
install.packages("plyr")
library(plyr)
library(dplyr)
# start_mlr$State <- revalue(start_mlr$State,c("New York"="0", "California"="1", "Florida"="2")) 
class(start_mlr)
 
attach(start_mlr)
summary(start_mlr)
plot(start_mlr)
pairs(start_mlr)
plot(Profit,Administration)
plot(Profit,R.D.Spend)
plot(Profit,Marketing.Spend)
plot(Profit,State)

install.packages("corpcor")
library(corpcor)


Model.Startups <- lm(Profit~R.D.Spend+Administration+Marketing.Spend+State,data = start_mlr)
summary(Model.Startups)


Model.Startups1 <- lm(Profit~R.D.Spend+log(Administration))
summary(Model.Startups1)


### Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(start_mlr, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

install.packages("car")
library(car)



install.packages("mvinfluence")
library(mvinfluence)

influence.measures(Model.Startups)

influenceIndexPlot(Model.Startups1, id.n=3) # Index Plots of the influence measures

influencePlot(Model.Startups, id.n=3) # A user friendly representation of the above



vif(Model.Startups)  # VIF is > 10 => collinearity


avPlots(Model.Startups, id.n=2, id.cex=0.7) # Added Variable Plots


FinalModel<-lm(Profit~R.D.Spend+log(Administration)+Marketing.Spend+State,data=start_mlr[-c(49,50),])

summary(FinalModel)

Profit_Predict <- predict(FinalModel,interval="predict")
Final <- cbind(start_mlr$RD_Spend,start_mlr$Administration,start_mlr$Marketing_Spend,start_mlr$State,start_mlr$Profit,Profit_Predict)



View(Final)
plot(FinalModel)


qqPlot(FinalModel, id.n=5) 


library(MASS)
stepAIC(FinalModel)



