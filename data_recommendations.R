


# GETTING STARTED

#load packages
load.libraries <- c('tidyverse', 'ggplot2', 'data.table', 'cluster')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)



# import data
df <- read.csv(file = "datasets/wine.csv") 







# LOOK AT THE DATA



head(df)


#see which variables have missing data
apply(df, 2, function(r) any(r == ""))









# DATA CLEANING



# variable: reviewer

# remove data with no reviewers
df <- df[df$reviewer != "",]



# variable: alcohol

# change alcohol from char to double
df$alcohol <- parse_number(df$alcohol)

# remove alcohol values that do not make sense
df[df$alcohol >= 100 & !is.na(df$alcohol),]$alcohol <- NA



# variable: price

# see that price values with more than 6 characters are incorrect
head(df[nchar(df$price)>6,]$price)

# change correct price values to double and incorrect to NULL
df <- df %>% 
  mutate(price = ifelse(nchar(price) > 6, NA, parse_number(price)))



# variable: wine

# create index for duplicate wines
temp <- df[duplicated(df$wine),]$wine

# we see some wines have duplicate names, some of which have different categories, prices, alcohol or other variables.
# must assume that when other variables do not match, the wines are different
df[which(df$wine %in% temp), ] %>%  arrange(wine) %>% head()

# number of duplicate wine names
length(df[which(df$wine %in% temp), ]$wine)

# take a look at duplicate wines when there are are more than one reviewer
# looks like some of the wines face the same issues above, while some may actually be the same wine that were reviewed twice
check <- df[which(df$wine %in% temp), ] %>% group_by(wine) %>% 
  mutate(reviewer_count = length(unique(reviewer)) ) %>% 
  filter(reviewer_count >1) %>% 
  arrange(wine)

head(check)


# first, for duplicates, we will add the category to the wine name
df[(df$wine %in% temp), ]$wine <- paste(df[(df$wine %in% temp), ]$wine, df[(df$wine %in% temp), ]$category, sep = " - ") 



# index for duplicates
temp <- df[duplicated(df$wine),]$wine

# there are still duplicates
length(df[which(df$wine %in% temp), ]$wine)


# add the alcohol to the wine name
df[(df$wine %in% temp), ]$wine <- paste(df[(df$wine %in% temp), ]$wine, df[(df$wine %in% temp), ]$alcohol, sep = " - ") 


# new index for duplicate wines
temp <- df[duplicated(df$wine),]$wine

# there are still duplicate wine names
length(df[which(df$wine %in% temp),]$wine)

# add the price to the duplicate wine names
df[(df$wine %in% temp), ]$wine <- paste(df[(df$wine %in% temp), ]$wine, df[(df$wine %in% temp), ]$price, sep = " - $") 


# re-index for duplicate wines
temp <- df[duplicated(df$wine),]$wine

# there are duplicate wine names remaining
length(df[which(df$wine %in% temp), ]$wine)


# for the remaining duplicates, 
# it is assumed that reviewers made several reviews for the same wine. 
# We will remove all duplicate wines for the same reviewers


# first remove all duplicate wines with only one reviewer, 
#but leave one, unspecific observation for such wines as the observation for that wine

# add count of reviewers for each wine and a count of each wine,
# relative to the wine in each observation
df <- df %>% 
  group_by( wine) %>%
  mutate(count_user = length(unique(reviewer)), count_wine = n()) %>% 
  ungroup() 

# create new data set with only one observation for each wine..
# when the review count is one but multiple observations of a wine
dupwin_singleReviwer <- df %>% 
  filter(count_user == 1 & count_wine > 1) %>% 
  group_by(wine) %>% 
  slice(1) %>% 
  ungroup()


# remove all observations of wine in created data set from working data set
# then add created data set into working data set
df <- rbind(df[which(!df$wine %in% dupwin_singleReviwer$wine),], dupwin_singleReviwer)




#update wine and user count
df <- df %>% 
  group_by(wine) %>% 
  mutate(count_wine = n(), count_user = length(unique(reviewer))) %>%
  ungroup()


# make a df to list reviewers and their wine when there are more wine observations than
# unique reviewers for such wine
target_wine_list <- df %>% 
  filter(count_wine > count_user) %>%
  arrange(varietal) %>% 
  group_by(reviewer) %>% 
  summarize(wine) %>% 
  ungroup()


# pull only the data that matches are targeted users and wine and store in key_wine_data  
key_wine_data <- df[which(df$reviewer %in% target_wine_list$reviewer),] 

key_wine_data <- key_wine_data[which(key_wine_data$wine %in% target_wine_list$wine),]



# take only one review from each reviewer for each unique wine reviewed by a reviewer
key_wine_data <- key_wine_data %>% 
  mutate(target_key = paste(wine, reviewer, sep = " ")) %>% 
  group_by(target_key) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-target_key)


# replace data, ensuring remaining duplicates are among different reviewers for the same wine
df <- df %>% 
  filter(count_wine <= count_user) %>% 
  rbind(key_wine_data) %>% 
  subset(select=-c(count_wine, count_user))


# variable: category

# convert category to a factor
df$category <- as.factor(df$category)


# VISUALIZE category
df %>% 
  group_by(rating) %>% 
  summarise(cases = n()) %>% 
  ggplot(aes(rating, cases)) + geom_col() + theme_minimal() 


# look at the number of reviews per reviewer
ratings_sum <- df %>% 
  group_by(reviewer) %>% 
  count()

summary(ratings_sum$n)









# IMPUTING MISSING DATA




#FUNCTION - START
#getmode function to find mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv <- uniqv[which(!is.na(uniqv))]  
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# FUNCTION - END


#variable: alcohol

# for missing alcohol, replace the mode of the wine in the same category
df <- df %>% 
  group_by(category) %>% 
  mutate(mode = getmode(alcohol)) %>% 
  ungroup() %>% 
  mutate(alcohol = ifelse(is.na(alcohol), mode, alcohol)) %>% 
  subset(select = -mode)

# no missing alcohol data
length(df[is.na(df$alcohol),]$alcohol)




# variable: price

# replace missing prices with the mode of the prices from the same winery
df <- df %>% 
  group_by(winery) %>% 
  mutate(mode = getmode(price)) %>% 
  ungroup() %>% 
  mutate(price = ifelse(is.na(price), mode, price)) %>% 
  subset(select = -mode)

# still missing pricing
length(df[is.na(df$price),]$price)

# replace remaining NA price with the mean of all prices
df[is.na(df$price), ]$price <- as.integer(mean(df$price, na.rm = TRUE))







# CREATE A CONTENT-BASED RECOMMENDATION SYSTEM




# create dissimilarity matrix 

# arrange data by highest ratings to look at highest rated wines
df <- df %>% 
  arrange(desc(rating))

# we will only be looking at a sample of the wine data
# variables for comparison: winery, category, alcohol, price
wine_distance <- df[c(1:10000), c(2, 3, 7, 8)]


wine_distance[,1] <- as.factor(wine_distance[,1])

# using Gower distance to create dissimilarity matrix
# add weights to variables
dissimilarity <- daisy(wine_distance, metric = "gower", weights = c(2, 3, 0.5, 1) )
dissimilarity <- as.matrix(dissimilarity)


# add matrix names
row.names(dissimilarity) <- df$wine[1:10000]

colnames(dissimilarity) <- df$wine[1:10000]



# take a look at dissimilarity matrix
dissimilarity[15:16,15:15]



# making recommendations

# We will make recommendations to user Alexander Peartree

reviewer_target <- "Alexander Peartree"

# find the data from target user in our sample data
user_wine <- df %>%
  filter(reviewer == reviewer_target & wine %in% df$wine[1:10000]) %>%
  arrange(desc(rating))


# preparing data for recommendations

df$wine = as.character(df$wine)

selected_wine <- user_wine[ , c("wine", "rating")]


# FUNCTION - START
# create function to find recommendations for user based on their rated wines
# function takes a data frame with the users wine and ratings from sample wines,
#  dissimilarity matrix, the data frame containing all data and 
# number of recommendations to be returned

recommend <- function(selected_wine, dissimilarity_matrix, 
                      wine, n_recommendations = 5){
  
  
  # find index for wines in dissimilarity matrix
  selected_wine_indexes <- which(colnames(dissimilarity_matrix) %in% selected_wine$wine)
  
  # collect names of wine
  names_temp <- colnames(dissimilarity_matrix[,selected_wine_indexes])
  
  # find similarities to all wines with selected wines
  results <-  data.frame(dissimilarity_matrix[, selected_wine_indexes], 
                         recommended_wine = row.names(dissimilarity_matrix),
                         stringsAsFactors = FALSE) 
  
  # apply stored names to results
  names(results)[1:ncol(results)-1] <- names_temp 
  
  # find recommendations using results
  # includes finding a weighted score based on user reviews
  recommendations <- results %>%
    pivot_longer(cols = c(-"recommended_wine") , names_to = "rated_wine", 
                 values_to = "dissimilarity") %>%
    left_join(selected_wine, by = c("rated_wine" = "wine"))%>%
    arrange(desc(dissimilarity)) 
  
  recommendations <- recommendations[which(!recommendations$recommended_wine %in% recommendations$rated_wine),]
  
  recommendations <- recommendations %>%
    filter(recommended_wine != rated_wine) %>%
    filter(!is.na(rating) ) %>%
    mutate(
      similarity = 1 - dissimilarity,
      weighted_score = similarity * rating) %>%
    arrange(desc(weighted_score)) %>%
    filter(weighted_score>0) %>%
    group_by(recommended_wine) %>% slice(1) %>%
    ungroup() %>% 
    top_n(n_recommendations, weighted_score)  %>%
    left_join(df, by = c("recommended_wine" = "wine"))
  
  
  
  
  
  
  
  return(recommendations)
}
# FUNCTION-END

# use fuction to find recommendations
recommendations <- recommend(selected_wine, dissimilarity, df)

# using dissimilarity matrix and target user wine ratings, we can recommend other similar wines based on content:
recommendations[,1]







# CREATE ITEM-BASED COLLABORATIVE RECOMMENDING SYSTEM



# change to long data for user wine ratings
user_wine <- df %>%
  top_n(10000) %>%
  summarise(wine, reviewer, rating) %>% 
  pivot_wider(names_from = wine,values_from = rating) %>%
  as.data.frame()


row.names(user_wine) <- user_wine$reviewer
user_wine$reviewer = NULL

user_wine = as.matrix(user_wine)

# see results
user_wine[1:5,1:2]



# check the sparsity of matrix (0.952381)
# sparsity presents problems for collaboration recommendations
sum(is.na(user_wine)) /  ( ncol(user_wine) * nrow(user_wine) )

# FUNCTION - START
# function to find cosine similarity 
cos_similarity = function(A,B){
  num = sum(A *B, na.rm = T)
  den = sqrt(sum(A^2, na.rm = T)) * sqrt(sum(B^2, na.rm = T)) 
  result = num/den
  
  return(result)
}
# FUNCTION - END


# FUNCTION - START
# function to return recommendations based given wine by using cosine similarity
item_recommendation = function(wine_target, rating_matrix = user_wine, n_recommendations = 5){
  
  wine_index <- which(colnames(rating_matrix) == wine_target)
  
  similarity <- apply(rating_matrix, 2, FUN = function(y) 
    cos_similarity(rating_matrix[,wine_index], y))
  
  recommendations <- tibble(wine = names(similarity), 
                            similarity = similarity) %>%
    filter(wine != wine_target) %>% 
    top_n(n_recommendations, similarity) %>%
    arrange(desc(similarity)) 
  
  return(recommendations)
  
}
# FUNCTION - END


# find recommended wines
recom_cf_item <- item_recommendation("Recanati 2013 Reserve Petite Sirah (Galilee)")

# recommended wines  using item-based collaborative
head(recom_cf_item) 


