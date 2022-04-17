#setwd("/Users/ameesha/Desktop/CS_424/project3-taxi")

library(shiny)
library(ggplot2)
library(shinydashboard)
library(lubridate)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(dplyr)
library(plyr)
library(readr)
library(leaflet)
library(leaflet.providers)
library(viridis)

#get the file names with data
files = list.files(pattern="*.csv", full.name = T)

#read in the data
taxi_info = ldply(files, read_csv)
#print(head(taxi_info))
#print(nrow(taxi_info))

#fix dates using lubridate
taxi_info$TripDate = as_date(taxi_info$Trip_Date)

#add day, month and year data to csv
taxi_info$year = year(taxi_info$TripDate)
taxi_info$month = month(taxi_info$TripDate, abbr = TRUE, label = TRUE)
taxi_info$wday = wday(taxi_info$TripDate, label=TRUE)

print(head(taxi_info))
print(str(taxi_info))
print(min(taxi_info$TripDate))
print(max(taxi_info$TripDate))
print(min(taxi_info$Trip_Miles))
print(max(taxi_info$Trip_Miles))

m <- ggplot(taxi_info, aes(x=Trip_Seconds)) + 
  geom_bar(stat="bin", binwidth = 500, fill="#33647A") + 
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Total trip time", y ="Rides") + 
  theme_bw() +
  theme(text = element_text(family = "sans", face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))
m


df_new<- setNames(count(taxi_info$TripDate), c("Date", "Rides"))
head(df_new)

m <- ggplot(taxi_info, aes(x=Trip_Time)) + 
  geom_bar(stat="bin", binwidth = 5, fill="#33647A") + 
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Trip Time", y ="Number of Rides") + 
  theme_bw() 
m


g <- ggplot(taxi_info, aes(x = Trip_Miles)) + 
  geom_histogram(colour = 4, fill = "white", 
                 bins = 15)
g



#m <- m + geom_bar(stat="identity", width=0.7, fill="#33647A") +
#scale_fill_manual(values = c("(-Inf, 0]" = "#601e1e", "[1, Inf)" = "#153e51")) +
#scale_fill_gradient2(midpoint = 0, low = '#082b3a', high = '#490f0f') +
#m <- m + theme_bw() +
#  labs(x=paste("Station Name"), y="Total Entries") +
#  theme(text = element_text(family = "sans", face = "bold")) +
#  theme(axis.text.x = element_text(angle = 70, hjust=1))


