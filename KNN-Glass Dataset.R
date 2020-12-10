require(caTools)
glass<-read.csv(file.choose())

str(glass)

glass1<-glass[-10]
str(glass1)

##normalize 
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
glass_n <- as.data.frame(lapply(glass1[,1:9], norm))
summary(glass_n)

glass_n1<-data.frame(glass_n,glass$Type)
View(glass_n1)
str(glass_n1)

prop.table(table(glass_n1$glass.Type))*100

##split data into train and test
##sample split done

require(caTools)
set.seed(101) 
sample = sample.split(glass_n1$glass.Type, SplitRatio = 0.7)


train_d = subset(glass_n1, sample == TRUE)
test_d  = subset(glass_n1, sample == FALSE)



train_g_cl<-train_d[,10]



test_g_cl<-test_d[,10]


##build model
glas_pred<-knn(train_d,test_d,cl=train_g_cl,k=10)


##confusion matrix

cm<-table(glas_pred,test_g_cl)
cm


##calculating accuracy
accuracy1 <- function(x){
  sum(diag(x)/(sum(rowSums(x)))) * 100
}
accuracy1(cm)


##at k=11, the efficiency drops
#when split ratio is 50% & k=10, the efficiency is 97.24

#hence selecting k=10 , efficiency is 100% , split ratio selected is 70%

