# Repository at https://github.com/bryanpze/EC349_Yelp

# Load necessary libraries
library(ggplot2)    
library(jsonlite)
library(dplyr)
library(reshape2)
library(tm)
library(tidytext)
library(wordcloud)
library(caTools)
library(recipes)
library(textrecipes)
library(caret)
library(rpart) 
library(rpart.plot)
library(ranger)
# Clear the R Workspace
cat("\014")  
rm(list=ls())
gc()
setwd("E:/yelp_dataset")
set.seed(1)

# Load and save as RDS
# Loading from RDS is faster compared to JSON
business_data <- stream_in(file("./yelp_academic_dataset_business.json"))
saveRDS(business_data,file='yelp_academic_dataset_business.rds')
rm(business_data)
gc()

checkin_data <- stream_in(file("./yelp_academic_dataset_checkin.json"))
saveRDS(checkin_data,file='yelp_academic_dataset_checkin.rds')
rm(checkin_data)
gc()

tip_data <- stream_in(file("./yelp_academic_dataset_tip.json"))
saveRDS(tip_data,file='yelp_academic_dataset_tip.rds')
rm(tip_data)
gc()


# Load Files
load('yelp_review_small.Rda')
load('yelp_user_small.Rda')
business_data <- readRDS('./yelp_academic_dataset_business.rds')

# Exploratory Data Analysis
# Review Data
str(review_data_small)
summary(review_data_small)
head(review_data_small)
(colSums(!is.na(review_data_small))/nrow(review_data_small))*100
ggplot(review_data_small, aes(x = stars)) + geom_histogram(binwidth = 1, fill = "skyblue", color = "black") + geom_text(stat="count", aes(label = paste(after_stat(count), " (", sprintf("%.1f%%", after_stat(count)/sum(after_stat(count))*100), ")", sep = "")), position = position_stack(vjust = 0.5), color = "black", size = 3) + labs(title = "Distribution of Stars",x = "Stars",y = "Number Of Reviews")
cor_matrix <- cor(select(review_data_small, stars, useful, funny, cool))%>%melt()
ggplot(data=cor_matrix, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + labs(title = "Review Data Correlation Heatmap",x = "Variable 1",y = "Variable 2")

review_data_small$date<-as.Date(review_data_small$date)
review_data_small$year<-format(review_data_small$date,"%Y")
review_data_small$month<-format(review_data_small$date,"%m")
reviews_per_year<-review_data_small %>% group_by(year) %>% summarise(num_reviews=n())
reviews_per_month<-review_data_small %>% group_by(month) %>% summarise(num_reviews=n())
avg_stars_per_year<-review_data_small %>% group_by(year) %>% summarise(avg_stars=mean(stars))
avg_stars_per_month<-review_data_small %>% group_by(month) %>% summarise(avg_stars=mean(stars))
ggplot(reviews_per_year,aes(x = year,y = num_reviews, fill=year)) + geom_bar(stat = "identity") + labs(title="Number Of Reviews Each Year", x="Year",y="Number Of Reviews")
ggplot(reviews_per_month,aes(x = month,y = num_reviews, fill=month)) + geom_bar(stat = "identity") + labs(title="Number Of Reviews Each Month", x="Month",y="Number Of Reviews") + scale_x_discrete(labels=month.abb)
ggplot(avg_stars_per_year,aes(x = year,y = avg_stars, fill=year)) + geom_bar(stat = "identity") + labs(title="Average Stars Each Year", x="Year",y="Average Stars")
ggplot(avg_stars_per_month,aes(x = month,y = avg_stars, fill=month)) + geom_bar(stat = "identity") + labs(title="Average Stars Each Month", x="Month",y="Average Stars") + scale_x_discrete(labels=month.abb)

reviews_low <- review_data_small$text[review_data_small$stars == 1]
corpus_low <- Corpus(VectorSource(reviews_low))%>%tm_map(tolower)%>%tm_map(removePunctuation)%>%tm_map(removeNumbers)%>%tm_map(removeWords, stopwords("en"))
#corpus_low <- DocumentTermMatrix(corpus_low)
#corpus_low <- removeSparseTerms(corpus_low, 0.99)
#colnames(corpus_low)
#corpus_low = as.data.frame(as.matrix(corpus_low))
corpus_low <- data.frame(text=sapply(corpus_low, as.character), stringsAsFactors=FALSE)
words <- corpus_low %>%unnest_tokens(word, text)
word_freqs <- words %>%anti_join(stop_words) %>%count(word, sort=TRUE)
wordcloud(words = word_freqs$word, freq = word_freqs$n, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0, colors=brewer.pal(8, "Dark2"))
title(main="Word Cloud for Low Rating (Stars == 1)")

reviews_high <- review_data_small$text[review_data_small$stars == 5]
corpus_high <- Corpus(VectorSource(reviews_high))%>%tm_map(tolower)%>%tm_map(removePunctuation)%>%tm_map(removeNumbers)%>%tm_map(removeWords, stopwords("en"))
corpus_high <- data.frame(text=sapply(corpus_high, as.character), stringsAsFactors=FALSE)
words <- corpus_high %>%unnest_tokens(word, text)
word_freqs <- words %>%anti_join(stop_words) %>%count(word, sort=TRUE)
wordcloud(words = word_freqs$word, freq = word_freqs$n, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0, colors=brewer.pal(8, "Dark2"))
title(main="Word Cloud for High Rating (Stars == 5)")

# Business Data
str(business_data)
summary(business_data)
head(business_data)
(colSums(!is.na(business_data))/nrow(business_data))*100
cor_matrix <- cor(select(business_data, stars, review_count, latitude, longitude))%>%melt()
ggplot(data=cor_matrix, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + labs(title = "Business Data Correlation Heatmap",x = "Variable 1",y = "Variable 2")

total_reviews_by_city <- business_data %>%group_by(city) %>%summarise(total_reviews = sum(review_count, na.rm = TRUE)) %>%arrange(desc(total_reviews)) %>%head(30)
five_star_cities <- business_data%>%filter(stars == 5) %>%group_by(city) %>% summarise(total_5_star_reviews = sum(review_count, na.rm = TRUE)) %>%arrange(desc(total_5_star_reviews)) %>%head(15) 
one_star_cities <- business_data%>%filter(stars == 1) %>%group_by(city) %>% summarise(total_1_star_reviews = sum(review_count, na.rm = TRUE)) %>%arrange(desc(total_1_star_reviews)) %>%head(15)
ggplot(total_reviews_by_city, aes(x = reorder(city, -total_reviews), y = total_reviews)) +geom_bar(stat = "identity", fill = "green") + labs(title = "Cities with the Most Total Reviews", x = "City", y = "Total Reviews") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))  
ggplot(five_star_cities, aes(x = reorder(city, -total_5_star_reviews), y = total_5_star_reviews)) +geom_bar(stat = "identity", fill = "blue") + labs(title = "Cities with the Most 5 Star Ratings", x = "City", y = "Total 5 Star Ratings") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))  
ggplot(one_star_cities, aes(x = reorder(city, -total_1_star_reviews), y = total_1_star_reviews)) +geom_bar(stat = "identity", fill = "red") + labs(title = "Cities with the Most 1 Star Ratings", x = "City", y = "Total 1 Star Ratings") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))  

total_reviews_by_category<- business_data %>%group_by(categories) %>%summarise(total_reviews = sum(review_count, na.rm = TRUE),avg_stars = mean(stars, na.rm = TRUE)) %>%arrange(desc(total_reviews)) %>%head(30)
top_avg_stars_categories <- total_reviews_by_category %>% arrange(desc(avg_stars)) %>% head(15)
bot_avg_stars_categories <- total_reviews_by_category %>% arrange((avg_stars)) %>% head(15)
ggplot(total_reviews_by_category, aes(x = reorder(categories, -total_reviews), y = total_reviews)) + geom_bar(stat = "identity", fill = "purple") + labs(title = "Most Popular Categories", x = "Category", y = "Total Reviews") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(top_avg_stars_categories, aes(x = reorder(categories, -avg_stars), y = avg_stars)) + geom_bar(stat = "identity", fill = "blue") + labs(title = "Categories with Highest Average Stars", x = "Category", y = "Average Stars") + theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(bot_avg_stars_categories, aes(x = reorder(categories, avg_stars), y = avg_stars)) + geom_bar(stat = "identity", fill = "red") + labs(title = "Categories with Lowest Average Stars", x = "Category", y = "Average Stars") + theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1))

# User Data
str(user_data_small)
summary(user_data_small)
head(user_data_small)
(colSums(!is.na(user_data_small))/nrow(user_data_small))*100
cor_matrix <- cor(select(user_data_small,review_count, useful, funny, cool, average_stars,compliment_hot,compliment_more,compliment_profile,compliment_cute,compliment_list,compliment_note,compliment_plain,compliment_cool,compliment_funny,compliment_writer,compliment_photos))%>%melt()
ggplot(data=cor_matrix, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + labs(title = "User Data Correlation Heatmap",x = "Variable 1",y = "Variable 2")+theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Clear everything to restart
cat("\014")  
rm(list=ls())
gc()
setwd("E:/yelp_dataset")
set.seed(1)

# Create data
load('yelp_review_small.Rda')
load('yelp_user_small.Rda')
business_data <- readRDS('./yelp_academic_dataset_business.rds')
length(unique(business_data$categories))
length(unique(business_data$city))
length(unique(business_data$categories))


data<-left_join(review_data_small,rename_with(user_data_small,~ ifelse(. != "user_id", paste0("user_", .), .)),by='user_id')%>%left_join(rename_with(business_data,~ ifelse(. != "business_id", paste0("business_", .), .)),'business_id')
rm(user_data_small)
rm(review_data_small)
rm(business_data)

# Remove business attributes,business hours column before splitting because recipe can't handle it for some reason
data[c('business_attributes','business_hours')] <- list(NULL)
# Split train, test data
split <- sample.split(data$stars,SplitRatio=0.8)
train <- data[split,]
test <- data[!split,]
nrow(train)
nrow(test)
nrow(data)
rm(data)
gc()

# Create Pipeline for Data Preprocessing
recipe <- recipe(~ ., data = train) %>%
  # Data Cleaning
  step_rm(user_id, business_id, review_id, user_name, user_elite, user_friends, business_name, business_address, business_postal_code, business_is_open,business_latitude,business_longitude,business_categories,business_city,business_state,user_yelping_since) %>%
  # Factor Columns
  # Can not handle categorical predictors with more than 53 categories.
  #step_string2factor(business_categories,business_city,business_state)  %>%
  # Feature Engineering - Date Columns
  step_mutate(date = as.Date(date))  %>%
  step_date(date) %>% 
  step_rm(date) %>%
  # Feature Engineering - Review Text
  step_tokenize(text) %>%
  step_stopwords(text) %>%
  step_tokenfilter(text, max_tokens = 100) %>%  
  step_tfidf(text)%>%
  # Impute Median values for user and review features
  # Remove missing values rows and zero variance predictors
  step_impute_median(all_numeric())%>%
  #step_unknown(business_categories,new_level="unknown category")%>%
  #step_unknown(business_city,new_level="unknown category")%>%
  #step_unknown(business_state,new_level="unknown category")%>%
  step_zv(all_predictors())
prep_recipe <- prep(recipe)
train<- bake(prep_recipe,new_data = NULL)
test <- bake(prep_recipe, new_data = test)
rm(recipe)
gc()
colnames(train)

set.seed(1)

#Baseline model
lm.fit <- lm(stars~.,data=train)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit$residuals)
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))
plot(train$stars,predict(lm.fit))
par(mfrow=c(1,1))

linear_train <- mean((train$stars - predict(lm.fit,newdata = train))^2)
linear_train
linear_test <- mean((test$stars - predict(lm.fit,newdata = test))^2)
linear_test

rm(lm.fit)
rm(linear_train)
rm(linear_test)
gc()

# Decision Tree Model
decision_tree<- rpart(stars~.,data=train,method='anova')
rpart.plot(decision_tree)
tree_train <- mean((train$stars - predict(decision_tree,newdata = train))^2)
tree_train
tree_test <- mean((test$stars - predict(decision_tree,newdata = test))^2)
tree_test

rm(decision_tree)
rm(tree_train)
rm(tree_test)
gc()

# Decision Tree With Cross Validation
numFolds <- trainControl(method = "cv", number = 10)
paramGrid <- expand.grid(.cp=seq(0.0005, 0.005, 0.0005))
fit_dt <- train(stars~., data = train, method = "rpart", trControl = numFolds, tuneGrid = paramGrid)
fit_dt
plot(fit_dt)
varImp(fit_dt)
decision_tree_optimized <- rpart(stars~.,data=train,method='anova',cp=0.0005)
tree_optimized_train <- mean((train$stars - predict(decision_tree_optimized,newdata = train))^2)
tree_optimized_train
tree_optimized_test <- mean((test$stars - predict(decision_tree_optimized,newdata = test))^2)
tree_optimized_test

rm(numFolds)
rm(paramGrid)
rm(fit_dt)
rm(decision_tree_optimized)
rm(tree_optimized_train)
rm(tree_optimized_test)
gc()

# Random Forest with Cross Validation
#hyper_grid <- expand.grid(
#  mtry = floor(length(colnames(train)) * c(.05, .15, .25, .333, .4)),
#  num.trees = floor(length((train))*c(10,5,1,0.5)),
#  min.node.size = c(1, 3, 5, 10), 
#  replace = c(TRUE, FALSE),                               
#  sample.fraction = c(.5, .63, .8),                       
#  rmse = NA                                               
#)
fit_rf <- ranger(stars~.,data=train, num.trees = 500 ,verbose = TRUE,seed = 1)
rf_train <- mean((train$stars - predict(fit_rf,data = train)$predictions)^2)
rf_train
rf_test <- mean((test$stars - predict(fit_rf,data = test)$predictions)^2)
rf_test