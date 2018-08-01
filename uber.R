apr14<-read.csv('D:/rdocs/machine learing algorithms/uber_kmeans/uber-raw-data-apr14.csv')
View(apr14)
may14<-  read.csv('D:/rdocs/machine learing algorithms/uber_kmeans/uber-raw-data-may14.csv')
june14<-read.csv('D:/rdocs/machine learing algorithms/uber_kmeans/uber-raw-data-jun14.csv')
july14<-read.csv('D:/rdocs/machine learing algorithms/uber_kmeans/uber-raw-data-jul14.csv')
aug14<-read.csv('D:/rdocs/machine learing algorithms/uber_kmeans/uber-raw-data-aug14.csv')
sep14<-read.csv('D:/rdocs/machine learing algorithms/uber_kmeans/uber-raw-data-sep14.csv')
library(dplyr)
dataset<-bind_rows(apr14,may14,june14,july14,aug14,sep14)
summary(dataset)
library(lubridate)
dataset$Date.Time <- mdy_hms(dataset$Date.Time)
dataset$Year <- factor(year(dataset$Date.Time))
dataset$Month <- factor(month(dataset$Date.Time))
dataset$Day <- factor(day(dataset$Date.Time))
dataset$Weekday <- factor(wday(dataset$Date.Time))
dataset$Hour <- factor(hour(dataset$Date.Time))
dataset$Minute <- factor(minute(dataset$Date.Time))
dataset$Second <- factor(second(dataset$Date.Time))
head(dataset,n=10)
set.seed(20)
clusters <- kmeans(dataset[,2:3], 5)
str(clusters)
dataset$Borough <- as.factor(clusters$cluster)
library(ggmap)
library(ggplot2)
library(rworldmap)

NYCMap <- get_map("New York", zoom = 10)
ggmap(NYCMap) + geom_point(aes(x = Lon[], y = Lat[], colour = as.factor(Borough)),data = dataset) +
  ggtitle("NYC Boroughs using KMean")



library(DT)

dataset$Month <- as.double(dataset$Month)
month_borough_14 <- count_(dataset, vars = c('Month', 'Borough'), sort = TRUE) %>% 
  arrange(Month, Borough)
datatable(month_borough_14)

library(dplyr)
monthly_growth <- month_borough_14 %>%
  mutate(Date = paste("04", Month)) %>%
  ggplot(aes(Month, n, colour = Borough)) + geom_line() +
  ggtitle("Uber Monthly Growth - 2014")
monthly_growth
