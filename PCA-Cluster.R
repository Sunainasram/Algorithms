hc<-read.csv(file.choose())
str(hc)
fhc<-factor(hc$Type)
hcd<-data.frame(hc,fhc)
hcn<-hc[-1]
##hclustering


normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
normalized_data<-as.data.frame(lapply(hc[,2:14],normalize))
summary(normalized_data)

d<-dist(normalized_data,method = "euclidean")
d
fit<-hclust(d,method = "complete")
plot(fit,hang = -1)

groups<-cutree(fit,k=7)
rect.hclust(fit,k=7,border = "red")

membership<-as.matrix(groups)



final3 <- data.frame(hc, membership)
final3
final4 <- final3[,c(ncol(final),1:(ncol(final)-1))]

final4

agg<-aggregate(hc[,2:14], by=list(final4$membership), FUN=mean)
agg

#k means


wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
twss = c()
for (i in 2:14) twss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:14, twss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
View(twss)

?kmeans
kclust<-kmeans(normalized_data,6)
final<-data.frame(hc,kclust$cluster)



final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
final1
aggregate(hc[,2:14], by=list(final1$kclust.cluster), FUN=mean)


