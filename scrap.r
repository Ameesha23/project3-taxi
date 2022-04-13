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
taxi_info$day = day(taxi_info$TripDate)
taxi_info$wday = wday(taxi_info$TripDate, label=TRUE)
taxi_info$DayMonth <- format(as.Date(taxi_info$TripDate), "%d-%m")

print(head(taxi_info))
print(min(taxi_info$TripDate))
print(max(taxi_info$TripDate))

DateSub <- setNames(aggregate(taxi_info, by=list(taxi_info$Trip_Time), FUN=sum), c("date", "rides"))
print(head(DateSub))

m <- ggplot(taxi_info, aes(x=DayMonth)) + 
  geom_bar(stat="count", width=0.7, fill="#33647A") + 
  scale_y_continuous(labels = scales::comma)
m

#m <- m + geom_bar(stat="identity", width=0.7, fill="#33647A") +
#scale_fill_manual(values = c("(-Inf, 0]" = "#601e1e", "[1, Inf)" = "#153e51")) +
#scale_fill_gradient2(midpoint = 0, low = '#082b3a', high = '#490f0f') +
#m <- m + theme_bw() +
#  labs(x=paste("Station Name"), y="Total Entries") +
#  theme(text = element_text(family = "sans", face = "bold")) +
#  theme(axis.text.x = element_text(angle = 70, hjust=1))


