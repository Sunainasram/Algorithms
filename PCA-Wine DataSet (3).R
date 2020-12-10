wdata<-read.csv(file.choose())
View(wdata)


help("princomp")
##to remove the type column
View(wdata[-1])

newwdata<-wdata[-1]
newwdata
View(newwdata)

cor(newwdata)

pcaObj<-princomp(wdata[-1], cor = TRUE, scores = TRUE, covmat = NULL)
str(pcaObj)
screeplot(pcaObj)
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)

plot(pcaObj)

biplot(pcaObj)

plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
View(pcaObj$scores)
pcaObj$scores[,1:3]

newwdata<-cbind(wdata,pcaObj$scores[,1:3])
View(newwdata)

# preparing data for clustering (considering only pca scores as they represent the entire data)

cluster_data<-newwdata[,15:17]

###normalize the data
norm_data<-scale(cluster_data) 
View(norm_data)

#H clustering
distance<-dist(norm_data,method = "euclidean") 

fit<-hclust(distance,method="complete")
plot(fit,hang = -1)

groups<-cutree(fit,7)
rect.hclust(fit,k=7,border = "red")

membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)

final1<-cbind(membership_1,newwdata) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(2,16:18)],by=list(membership_1),FUN=mean)) 

###k means clustering
sdata<-scale(wdata[,2:14])


wss = (nrow(sdata)-1)*sum(apply(sdata, 2, var))		 # Determine number of clusters by scree-plot 
twss = c()
for (i in 2:14) twss[i] = sum(kmeans(sdata, centers=i)$withinss)
plot(1:14, twss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #

title(sub = "K-Means Clustering Scree-Plot")
View(twss)


?kmeans
kclust<-kmeans(sdata,7)
final<-data.frame(wdata,kclust$cluster)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]

final1
aggregate(wdata[,2:14], by=list(final1$kclust.cluster), FUN=mean)


write.csv(final1,file="wine_clustering1.csv",row.names = F,col.names =F)
getwd()
