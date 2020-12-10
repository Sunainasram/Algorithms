library(arules)
#load into sparse matrix 
groceries <- read.transactions("C:/Users/HP/Downloads/groceries.csv", sep = ",")
summary(groceries)
#inspect the first five transactions 
inspect(groceries[1:5])
#ITemsetfrequency
itemFrequency(groceries[, 1:3])
itemFrequencyPlot(groceries, support = 0.1)
#plot top 20 items in transactions
itemFrequencyPlot(groceries, topN = 20)
image(groceries[1:5])
image(sample(groceries, 100))


#train a model 
library(arules)
apriori(groceries)
groceryrules <- apriori(groceries, parameter = list(support =
                                                      0.005, confidence = 0.30, minlen = 3))
groceryrules <- apriori(groceries, parameter = list(support =
                                                      0.006, confidence = 0.25, minlen = 3))

groceryrules <- apriori(groceries, parameter = list(support =
                                                      0.007, confidence = 0.20, minlen = 3))
inspect(groceryrules[1:3])

#to imporove model - sort by lift ratio 
inspect(sort(groceryrules, by = "lift")[1:5])

#finding subset of rules containing soda 
sodarules <- subset(groceryrules, items %in% "soda")
inspect(sodarules[1:3])

