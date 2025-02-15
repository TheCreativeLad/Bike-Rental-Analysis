## WORKING WITH BIKE RANTALS DATA

# Link to the dataset: https://www.kaggle.com/datasets/rishabhbarman/bike-sharing-dateset


#Importing the data set for the daily bike rentals
day <- read.csv("C:/Users/Agba Xchanger/Documents/Datasets/bike_day.csv")
View(day)
attach(day)
str(day)
library(dplyr)
library(ggplot2)
library(ggpubr)

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

## Checking the effect of public holiday on the bicycles rented
# grouping the data into two, i.e holiday and no holiday
hol_group <- day %>% 
  group_by(holiday) %>% 
  summarise(cnt = sum(cnt))

# naming the levels of the Holiday
hol_group$holiday <- factor(hol_group$holiday, levels = c(0, 1), labels = c("No Holiday", "Public Holiday"))

# visualization with barplot
ggplot(hol_group, aes(x = holiday, y = cnt, fill = holiday)) +
  geom_bar(stat = "identity") + 
  labs(x = "Holiday", y = "bicycle Rented", title = "Relationship between Holiday and bicycle rented")


## Visualization of the days of the week vs the bicycles rented
# grouping the days
weekday_group <- day %>% 
  group_by(weekday) %>% 
  summarise(cnt = sum(cnt))

# naming the levels of the Weekdays
weekday_group$weekday <- factor(weekday_group$weekday, 
                                levels = c(0, 1, 2, 3, 4, 5, 6), 
                                labels = c("Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat"))

## visualization with barplot
ggplot(weekday_group, aes(x = weekday, y = cnt, fill = weekday)) +
  geom_bar(stat = "identity") +
  labs(x = "Days of the Week", y = "bicycle Rented", title = "bicycle Rented in each day of the week")


## Visualizing the days that Casuals rent bike the most
weekday_group <- day %>% 
  group_by(weekday) %>% 
  summarise(casual = sum(casual))

# naming the levels of the Weekdays
weekday_group$weekday <- factor(weekday_group$weekday, 
                                levels = c(0, 1, 2, 3, 4, 5, 6), 
                                labels = c("Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat"))

## visualization with barplot
ggplot(weekday_group, aes(x = weekday, y = casual, fill = weekday)) +
  geom_bar(stat = "identity") +
  labs(x = "Days of the Week", y = "bicycle Rented", title = "bicycle Rented in each day of the week by Casuals")

## Visualizing the days that Registered Users rent bike the most
weekday_group <- day %>% 
  group_by(weekday) %>% 
  summarise(registered = sum(registered))

# naming the levels of the Weekdays
weekday_group$weekday <- factor(weekday_group$weekday, 
                                levels = c(0, 1, 2, 3, 4, 5, 6), 
                                labels = c("Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat"))

## visualization with barplot
ggplot(weekday_group, aes(x = weekday, y = registered, fill = weekday)) +
  geom_bar(stat = "identity") +
  labs(x = "Days of the Week", y = "bicycle Rented", title = "bicycle Rented in each day of the week by Registered Users")


## Visualization of the working days vs the bicycles rented
# grouping the working days
workingday_group <- day %>% 
  group_by(workingday) %>% 
  summarise(cnt = sum(cnt))

# naming the levels of the Holiday
workingday_group$workingday <- factor(workingday_group$workingday, 
                                levels = c(0, 1), 
                                labels = c("Not Working Day", "Working Day"))

## visualization with barplot
ggplot(workingday_group, aes(x = workingday, y = cnt, fill = workingday)) +
  geom_bar(stat = "identity") +
  labs(x = "WorkingDay", y = "bicycle Rented", title = "Bicycles Rented vs Working Day/Not Working Day")


## The effect of the weather temperature on the Bicycles rented
# checking for the relationship
ggplot(day, aes(x = atemp, y = cnt)) +
  geom_point(stat = "identity", color = "brown") +
  geom_smooth(method = "lm") +
  stat_regline_equation(formula = cnt ~ atemp, position = "identity") +
  labs(title = "The effect of Weather Temperature on the Bicycles Rented", x = "Weather Temperature", y = "Bicyces Rented")

# Analyzing the relationship between weather temperature and the Bicycles rented using linear model
fit <- lm(cnt ~ atemp)
summary(fit)

## Performing the correlation test
cor.test(atemp, cnt)


## The effect of Humidity on the Bicycles rented
# checking for the relationship
ggplot(day, aes(x = hum, y = cnt)) +
  geom_point(stat = "identity", color = "brown") +
  geom_smooth(method = "lm") +
  stat_regline_equation(formula = cnt ~ hum, position = "identity") +
  labs(title = "The effect of Humidity on the Bicycles Rented", x = "Humidity", y = "Bicyces Rented")

# Analyzing the relationship between Humidity and the Bicycles rented using linear model
fit <- lm(cnt ~ hum)
summary(fit)

## Performing the correlation test
cor.test(hum, cnt)


## Visualization for Seasons
season_counts <- day %>%
  group_by(season) %>%
  summarise(cnt = sum(cnt))

## Labeling the seasons

season_counts$season <- factor(season_counts$season, 
                               levels =  c(1, 2, 3, 4), 
                               labels =  c("Winter", "Spring", "Summer", "Fall"))



## Plot using ggplot

ggplot(season_counts, aes(x = season, y = cnt, fill = season)) + 
  geom_bar(stat = "identity") +
  labs(x = "Seasons", y = "Total Bicycles Rented", title = "Total Bicycles rented in each Season")


## Visualization of the effect of weather conditions on the bicycles rented
# grouping the wweathersit
weathersit_group <- day %>% 
  group_by(weathersit) %>% 
  summarise(cnt = sum(cnt))

# naming the levels of the Holiday
weathersit_group$weathersit <- factor(weathersit_group$weathersit, 
                                      levels = c(1, 2, 3, 4), 
                                      labels = c("Clear, Few clouds, Partly cloudy, Partly cloudy", 
                                                 "Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist",
                                                 "Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds",
                                                 "Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog"))

## visualization with bar plot
ggplot(weathersit_group, aes(x = weathersit, y = cnt, fill = weathersit)) +
  geom_bar(stat = "identity") +
  labs(x = "Weather Condition", y = "bicycle Rented", title = "Effect of the weather condition on the Bicycles rented")


## Visualization for Months

month_counts <- day %>%
  group_by(mnth) %>%
  summarise(cnt = sum(cnt))

## Labeling the seasons

month_counts$mnth <- factor(month_counts$mnth, 
                            levels =  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                            labels =  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

## Bar Chart for the Bicycles Rented in each month
ggplot(month_counts, aes(x = mnth, y = cnt, fill = mnth)) +
  geom_bar(stat = "identity") +
  labs(x = "Months", y = "Total Bicycles Rented", title = "Total Bicycles rented in each Month")


## Plotting the relationship between Wind speed and Bicycles Rented
ggplot(day, aes(x = windspeed, y = cnt)) +
  geom_point(stat = "identity", position = "identity", color = "brown") +
  stat_regline_equation(formula = cnt ~ windspeed, position = "identity") +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between the Winspeed and the Bicycles Rented", x = "Windspeed", y = "Bicycle Rented")

## Performing the linear model analysis between the wind speed and the Bicycles Rented
fit <- lm(cnt ~ windspeed)
summary(fit)

## Performing the correlation test
cor.test(windspeed, cnt)

## Checking the ratio of Casual vs Registered Customers
sum(casual)/(sum(registered) + sum(casual))

sum(registered)/(sum(registered) + sum(casual))
