## WORKING WITH BIKE RANTALS DATA

#Importing the data set for the daily bike rentals
day <- read.csv("C:/Users/Agba Xchanger/Documents/Datasets/bike_day.csv")
View(day)
attach(day)
str(day)
library(dplyr)

## DATA CLEANING

##turning season, yr, mnth, holiday, weekday, workingday, and wethersit into a factor

day$season <- as.factor(season)
day$yr <- as.factor(yr)
day$mnth <- as.factor(mnth)
day$holiday <- as.factor(holiday)
day$weekday <- as.factor(weekday)
day$workingday <- as.factor(workingday)
day$weathersit <- as.factor(weathersit)


## ANALYSIS BEGINS
## Getting the five-figure summary
summary(day)
summary(day$mnth)

## Visualization for Seasons

season_counts <- day %>%
  group_by(season) %>%
  summarise(cnt = sum(cnt))

## Labeling the seasons

season_counts$season <- factor(season_counts$season, 
                               levels =  c(1, 2, 3, 4), 
                               labels =  c("Winter", "Spring", "Summer", "Fall"))

season_counts


## Plot using ggplot
library(ggplot2)
library(ggpubr)

ggplot(season_counts, aes(x = season, y = cnt, fill = season)) + 
  geom_bar(stat = "identity") +
  labs(x = "Seasons", y = "Total Bicycles Rented", title = "Total Bicycles rented in each Season")


## Visualization for Seasons

month_counts <- day %>%
  group_by(mnth) %>%
  summarise(cnt = sum(cnt))

## Labeling the seasons

month_counts$mnth <- factor(month_counts$mnth, 
                            levels =  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                            labels =  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

## Bar Chart for the Bycicles Rented in each month
ggplot(month_counts, aes(x = mnth, y = cnt, fill = mnth)) +
  geom_bar(stat = "identity") +
  labs(x = "Months", y = "Total Bicycles Rented", title = "Total Bicycles rented in each Month")


## Plotting the relationship between Windspeed and Bycicles Rented
ggplot(day, aes(x = cnt, y = windspeed)) +
  geom_point(stat = "identity", position = "identity", color = "brown") +
  stat_regline_equation(formula = windspeed ~ cnt, position = "identity") +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between the Winspeed and the Bicycles Rented", x = "Bycicles Rented", y = "Windspeed")
