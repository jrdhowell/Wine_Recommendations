# Wine Recommendations

* Created systems to make content-based and item-based recommendations based on user reviews that predicts whether a shipment will be accepted or rejected in an effort to help the small business reduce losses from rejected shipments.
* Data used is from a public Kaggle data set

## Code and Resources Used
**R Version:** 4.1.2 (2021-11-01) <br/>
**Packages:** tidyverse, ggplot2, data.table, cluster <br/>
**Data:** https://www.kaggle.com/datasets/samuelmcguire/wine-reviews-data 

## Data Cleaning

* Cleaned variables reviewer, alcohol, price, wine and category. 
* Cleaning steps included converting to correct data type, removing incorrect data.
* Duplicate wine observations were heavily scrutinized to determine if the observations were for the same wine or represented different wines with repeated name.

## Data Visualization

* The shape of the rating variable data was visualized.

## Data Imputing

* Imputed missing data in alcohol and price variable. 
* The mode for the alcohol of wines in the same catogory was used. 
* The mode of the wine price at the same winery was first used and then the mean of all wine price was used as replacement data for price.

## Recommendations

* Created content-based recommendation system:
* Selected variables, created dissimilarity matrix using Gower distance due to factor and numerical variables
* Created function to return wine recommendations of a user based on the user's recommendations weighted by their ratings
<br> <br>
* Created item-based collaborative recommendation system:
* Pivoted the wine ratings, checked the sparsity (0.952), created function to make wine-based recommendations using cosine similarity
<br> <br>
* Created user-based collaborative recommendation system:
* Created function to make user-based recommendations based on cosine similarity
* Results for user-based recommendations were unusable due to sparsity

