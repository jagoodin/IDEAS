# This program is developed to perform Exercise 5 in the IDEAS module
# Created by James Goodin "jagoodin@uga.edu"

#Load packages
library(ggplot2)
library(tidyverse)
library(lubridate)

#Data
mers <- read.csv('cases.csv')

#Analysis

head(mers)
class(mers$onset)

mers$hospitalized[890] <- c('2015-02-20')
mers <- mers[-471,]


mers$onset2 <- ymd(mers$onset)
mers$hospitalized2 <- ymd(mers$hospitalized)
class(mers$onset2)

day0 <- min(na.omit(mers$onset2))

mers$epi.day <- as.numeric(mers$onset2 - day0)

ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")


