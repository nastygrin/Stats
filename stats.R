#Author: Niskarsh Gautam
#Topic: handling missing values

library(httr)
library(jsonlite)
library(dplyr)

# the API request to collect the data
# the API was used by me for the previous assignment for building the dataset
url = paste("https://api.themoviedb.org/3/movie/top_rated?api_key=720b2d53f98b38b7ff8e6db4d31cdfd4&language=en-US&page=1")
data1 <- fromJSON(url)
xyz = as.data.frame(data1)

for(page in c(2:100)) {
  url1 = paste("https://api.themoviedb.org/3/movie/top_rated?api_key=720b2d53f98b38b7ff8e6db4d31cdfd4&language=en-US&page=",page)
  data2 <- fromJSON(url1)
  dfs2 = as.data.frame(data2)
  xyz<-rbind(xyz,dfs2)
}
df=subset(xyz, select=-c(results.adult,page,results.backdrop_path,results.genre_ids,results.original_title,results.overview,results.poster_path,results.video,total_pages,total_results))

df %>% distinct(df$results.original_language)
# selecting a column to do the statistical analysis
movieRating <- df$results.vote_average

# computing mean 
meanRating<-function(y){
  sum<-0
  for(i in 1:length(y)){
    sum<-sum+y[i]
  }
  print(sum/length(y))
}
meanRating(movieRating)
# verifying mean value with respect to predefined function mean()
mean(movieRating)

# computing median 
medianRating <- function(y) {
  n <- length(y)
  s <- sort(y)
  ifelse(n%%2==1,s[(n+1)/2],(s[n/2]+s[(n/2)+1])/2)
}
medianRating(movieRating)

# verifying median value with respect to predefined function median()
median(movieRating)
#computing mode by method 1 (it only considers integer values)
which.max(tabulate(movieRating))

# verifying mode value with respect to predefined function mfv() under modeest library 
library(modeest, warn.conflicts = F)
mfv(movieRating)

#function to calculate inter quartile range
movieIqr<-function(y){
  n<-length(y)
  s<- sort(y)
  q1=y[(n+1)/4]   # first quartile
  q3=y[3*(n+1)/4] # third quartile
  Iqr=q3-q1  # inter quartile range = q3-q1
  print(abs(Iqr))
}
movieIqr(movieRating)

# verifying inter quartile range value with respect to predefined function iqr()
IQR(movieRating)

# calculating standard deviation 
sdRating<-function(y){
  sum<-0
  for(i in 1:length(y)){
    sum<-sum+(y[i]-mean(y))^2/(length(y)-1)
  }
  print(sqrt(sum))
}
sdRating(movieRating)

# verifying standard deviation value with respect to predefined function sd()
sd(movieRating)

# Probability values on Empirical  Rule using my user defined functions
mu<- meanRating(movieRating)
sigma<- sdRating(movieRating)

# probablity of mu-sigma
pnorm(mu-sigma)
pnorm(mu+sigma)

# probabilty of mu-2*sigma and mu+2*sigma
pnorm(mu-2*sigma)
pnorm(mu+2*sigma)

# probabilty of mu-3*sigma and mu+3*sigma
pnorm(mu-3*sigma)
pnorm(mu+3*sigma)

# vector with values computed by me
computedVector <- c(meanRating(movieRating), medianRating(movieRating), movieIqr(movieRating), sdRating(movieRating))
computedVector

# vector with values computed by built-in R functions for the same 
builtinVector <- c(mean(movieRating), median(movieRating), IQR(movieRating), sd(movieRating))
builtinVector

# plotting line graph for my self defined functions
plot(computedVector,type = "o", col="red")

# plotting line graph for built-in functions
plot(bulitinVector,type = "o", col="blue")

# plotting both the graphs together 
plot(computedVector,type = "o", col="red")
lines(builtinVector, type = "o", col = "blue")
# we see only the blue line because both the graphs overlap

# HYPOTHESIS TESTING

# subsetting the data for the hypothesis testing 
dfe<-df[df$results.original_language == 'en', c(2,6)] # English movies
dfj<-df[df$results.original_language == 'ja', c(2,6)] # Japanese movies

head(dfj)
head(dfe)

summary(dfj)
summary(dfe)

### NULL HYPOTHESIS: 
# H0: There is no significant difference between ratings of Japanese and English movies

### Applying t-Test
result <- t.test(sample(dfe$results.vote_average,100), sample(dfj$results.vote_average,100), var.equal=T)
result

#plotting t-Test
library(webr)
plot(result)

### OBSERVATIONS:

# df- degree of freedom (n-1 per sample)
# p-vale < alpha (Hence, H0 can be rejected)
# alpha = 0.05

### CONCLUSION:

# ALTERNATE HYPOTHESIS: Ha: There is a significant difference between Japanese and English movies with respect to movie ratings.
# The Japanese movies have got better ratings when compared with English movies
