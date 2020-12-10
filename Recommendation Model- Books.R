recommendation_data<-read.csv(file.choose())
str(recommendation_data)
recommendation_data<-recommendation_data[-1]
View(recommendation_data)
str(recommendation_data)

###to check rating distribution
hist(recommendation_data$Book.Rating)

install.packages("recommenderlab")
library(recommenderlab)

##convert data into real rating matrix
book_rec<-as(recommendation_data, 'realRatingMatrix')
book_rec

###model building using POPULAR method

book_recommendation1_model<-Recommender(book_rec,method="POPULAR")

##predict the top rated book a user 

recommend_books<-predict(book_recommendation1_model,book_rec[1010],n=1)
recommend_books

as(recommend_books,"list")

######UBCF###

book_recommendation1_model1<-Recommender(book_rec,method="UBCF")
book_recommendation1_model1

#predict for a user the top 5 books 
recommend_books1<-predict(book_recommendation1_model1,book_rec[1234],n=5)
recommend_books1

as(recommend_books1,"list")


##Inference 
##"In the Beauty of the Lilies" Book is recommended based on the ratings