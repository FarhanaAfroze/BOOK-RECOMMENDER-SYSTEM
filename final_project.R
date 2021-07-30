setwd("/Users/shifa/Documents")
rm(list=ls())
library(recommenderlab)
library(dplyr)
library(knitr)
library(ggplot2)                      
library(data.table)
library(reshape2)



book_data = read.csv("Books.csv")
View(book_data)
rating_data = read.csv("Ratings.csv")
View(rating_data)
user_data = read.csv("Users.csv")
View(user_data)

##### preprocessing the data set ########

### merging the data set

data_f = merge(book_data, rating_data, by = "ISBN")
View(data_f)

final_data = merge(data_f, user_data, by = "User.ID")
View(final_data)


### fining any missing values #######
missing_val = sum(is.na(final_data))
missing_val

## omitting missing value rows ####
final_data = na.omit(final_data)
sum(is.na(final_data))

dim(final_data)

View(final_data)


# next removing columns

first_data = subset(final_data, select= -c(ISBN, Book.Author, Year.Of.Publication, Publisher, Location,Age))
dim(first_data)

#first_data = first_data[first_data$Book.Rating != 0, ]
first_data = subset(first_data, select= -c(Image.URL.S, Image.URL.M, Image.URL.L))
dim(first_data)

View(first_data)

sum(is.na(first_data))

#### remove duplicate first(would like to remove rows where user rated 1 book twice ) ######

first_data <- first_data[!duplicated(first_data[,c(1,2)]),]
dim(first_data)

first_data$User.ID <- as.factor(first_data$User.ID)
is.factor(first_data$User.ID)


######## would like to remove user who rated less than 25 books #########
first_data  = first_data %>% group_by(User.ID) %>% filter(n()>=25) %>% ungroup()
View(first_data)


########## Visualization of our dataset ######

### would like to see frequency of ratings

ratings = table(first_data$Book.Rating)
ratings
quartz()
barplot <- barplot(ratings, main = "Users ratings of books", col = "red", xlab = "Ratings" , ylim = c(0, 550000), ylab = "count")     
text(x = barplot,  y = ratings + 14000, labels = ratings)


#### number of ratings per user #####

first_data %>% 
  group_by(User.ID) %>% 
  summarize(number_of_ratings_per_user = n()) %>% 
  ggplot(aes(number_of_ratings_per_user)) + 
  geom_bar(fill = "cadetblue3", color = "blue") + coord_cartesian(c(25, 100))


####################### subset of data ####################
set.seed(1)

first_data =  first_data %>% sample_frac(.1)

first_data  = first_data %>% group_by(User.ID) %>% filter(n()>=20) %>% ungroup()
View(first_data)

ratings = table(first_data$Book.Rating)
ratings

first_data %>% 
  group_by(User.ID) %>% 
  summarize(number_of_ratings_per_user = n()) %>% 
  ggplot(aes(number_of_ratings_per_user)) + 
  geom_bar(fill = "cadetblue3", color = "blue") + coord_cartesian(c(25, 100))



######################  RECOMMENDER SYSTEM STARTS #################
install.packages("reshape2")
library(reshape2)

matt <- acast(first_data, User.ID~ Book.Title)
matri <- as.matrix(matt)
rating_book <- as(matri, "realRatingMatrix")
rating_book

nrow(rating_book)
ncol(rating_book)

head(names(colCounts(rating_book)))

##############  looking through matrix  and some visualization ############
getRatingMatrix(rating_book[c(1:10)])

# As our matrix is very sparse we would like to normailze it because there are some useres who wil give low rating and other will give high rating

norm_rating = normalize(rating_book)

getRatingMatrix(norm_rating)[1:10,1:10]

?getRatings
hist(getRatings(norm_rating), breaks=100, xlim = c(-6,6), main = "Normalized Histogram")

ratingg = as.vector(rating_book@data)
ratinging = table(ratingg)
ratinging

# user average ratings
average_rating_user = rowMeans(rating_book)
hist(average_rating_user, breaks = 50, xlim = c(0,10), main = "Distribution of the average rating per user")

# book average ratings
book_average_rate = colMeans(rating_book)
hist(book_average_rate, breaks = 10, main = "Average rating per books")


image(rating_book[1:65, 1:65])

########### look some information about reccommender lab ###########
model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(model)

lapply(model, "[[", "description")

model$IBCF_realRatingMatrix$parameters
#######################################################################


############ data preparation for reccomender system #############

ratings_bookk = rating_book[rowCounts(rating_book) > 7, colCounts(rating_book) > 5]
ratings_bookk

ratings_book = ratings_bookk[rowCounts(ratings_bookk)>7]
ratings_book



############## let's see similliratiy matrix ###########
user_sim <- similarity(ratings_book[1:20,], method = "cosine", which = "users")
image(as.matrix(user_sim), main = "User Similarity")

item_sim <- similarity(ratings_book[ ,1:20], method = "cosine", which = "items")
image(as.matrix(item_sim), main = "Item Similarity")


image(ratings_book[1:50, 1:50], main = "First 50 users and books") #non-binarized
# here darker spot represents highest ratings books

getRatingMatrix(ratings_book)[1:50, 1:40]



############ Recommender algorithm #############

########### popular algorithm ##############

popular_books = Recommender(ratings_book[1:100], method = "POPULAR" )
popular_books

names(getModel(popular_books))
getModel(popular_books)$topN


recom <- predict(popular_books, ratings_book[101:115], n=5)
recom
as(recom, "list")


############ UBCF algorithm ##############

getRatingMatrix(ratings_book)[20:80, 1:5]

current_user = '258534'
current_user3 = '227447'
current_user4 = '143175'

ubcf_model = Recommender(data = ratings_book, method = 'UBCF', param = list(method = "pearson", nn = 5))
ubcf_model

prediction <- predict(ubcf_model, ratings_book[current_user, ], type = "ratings")
prediction

as(prediction, "list")



prediction2 <- predict(ubcf_model, ratings_book[current_user3, ], type = "ratings")
prediction2

as(prediction2, "list")

prediction4 <- predict(ubcf_model, ratings_book[current_user4, ], type = "ratings")
prediction4

as(prediction4, "list")



####### splitting data into test and train ########

(min(rowCounts(ratings_book)))
splits = sample(x=c(TRUE,FALSE), size = nrow(ratings_book), replace = TRUE, prob = c(0.75, 0.25))

train_data = ratings_book[splits,]
train_data
test_data = ratings_book[!splits,]
test_data

############## IBCF############

ibcf_model = Recommender(data = train_data, method = 'IBCF', parameter = list(k = 30))
ibcf_model


ibcf_predict <- predict(object = ibcf_model, newdata = test_data, n = 5)
ibcf_predict

ibcf_predict@items[[2]]

recc_matrix <- lapply(ibcf_predict@items, function(x){
  colnames(rating_book)[x]
})

recc_matrix[1:5]


########## comparing differernt recommender algorithms to see how performing diffrent algorithm ##########

set.seed(1)
scheme1<- evaluationScheme(ratings_book, method="cross", k=3, given= 4, goodRating=4)
scheme1
algorithms<- list("random items" = list(name= "RANDOM", param=NULL),
                  "popular items"= list(name = "POPULAR", param =NULL),
                  "item-based CF"= list(name = "IBCF", param = list(k=30)))
                  
results<- evaluate(scheme1, algorithms, type="topNList", n=c(1,3,5,10,15,20,25,30))
results
names(results)
quartz()
plot(results, annotate=c(1,3), legend="topleft")
quartz()
plot(results, "prec/rec", annotate = TRUE)

kable(head(getConfusionMatrix(results$`item-based CF`)[1]))
kable(head(getConfusionMatrix(results$`popular items`)[1]))
kable(head(getConfusionMatrix(results$`random items`)[1]))










