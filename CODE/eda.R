getwd()
setwd("/Users/Vivek/Downloads")
food <- read.csv("Reviews.csv")

library(dplyr)
library(caret)
library(magrittr)
library(stringr)
library(tidyverse)
library(knitr)
library(tm)
library(readr)

names(food)

a <-food$UserId
b <- unique(food$ProductId)
Reviews = mutate(food, PercentHelpful = HelpfulnessNumerator/HelpfulnessDenominator*100)

missing_data = Reviews %>%
  map_df(function(x) sum(is.na(x))) %>%
  gather(feature, num_nulls)%>%
  arrange(desc(num_nulls))%>%
  mutate(percent_missing = num_nulls/nrow(Reviews)*100)

kable(missing_data, digits = c(0,0,0))

#hist of scores
p1 <- ggplot(Reviews, aes(x=Score)) + geom_histogram(binwidth = 1, color= 'white', fill = "blue") + 
  coord_cartesian(xlim = c(.5, 5.5))+
  labs(title ="Histogram of Scores From 1-5 Given by Reviewer", x = "Score Given", y = "Number  of Reviews") +
  scale_x_continuous(breaks=seq(1,5, by = 1))
p1 + scale_y_continuous(labels = scales::comma)
table(Reviews$Score)
#top 10 products
a <- sort(table(Reviews$ProductId),decreasing=T)
x <- head(a, 10)
x <- as.data.frame(x)
#top 10 users
b <- sort(table(Reviews$UserId),decreasing=T)
y <- head(b, 10)
y <- as.data.frame(y)
