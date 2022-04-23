#setwd("/Users/ameesha/Desktop/CS_424/project3-taxi")

library(shiny)
library(ggplot2)
library(shinydashboard)
library(lubridate)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(plyr)
library(dplyr)
library(readr)
library(leaflet)
library(leaflet.providers)
library(viridis)
library(measurements)
library(sf)
library(rgdal)

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

taxi_info$Trip_km = conv_unit(taxi_info$Trip_Miles, from = "mi", to = "km")
taxi_info$km2 = taxi_info$Trip_Miles * 1.60934
head(taxi_info)

#separate a df with pickup areas and their corresponding rides
pickups_comm <- setNames(count(taxi_info$Pickup_Community_Area), c("area_num_1", "Rides"))
drop_comm <- setNames(count(taxi_info$Dropoff_Community_Area), c("area_num_1", "Rides"))

#information about the community areas
chi_map <- read_sf("https://raw.githubusercontent.com/thisisdaryn/data/master/geo/chicago/Comm_Areas.geojson") 
chi_map$area_num_1 = as.numeric(chi_map$area_num_1)
pickup_map <- left_join(chi_map, pickups_comm, by = ("area_num_1"))
dropoff_map <- left_join(chi_map, drop_comm, by = ("area_num_1"))

chi_map <- chi_map[order(chi_map$area_num_1),]

print(typeof(chi_map))
head(chi_map)

#read using rgdal
chiMapSP <- rgdal::readOGR("https://raw.githubusercontent.com/thisisdaryn/data/master/geo/chicago/Comm_Areas.geojson")
print(typeof(chiMapSP))

#make a menu for displaying the menu for selecting community areas
community_menu <- data.frame(chi_map$community, chi_map$area_num_1)
names(community_menu) <- c("community", "area_num_1")
community_menu <- community_menu[order(community_menu$community),]
print(community_menu[which(community_menu$community == "ALBANY PARK"), "area_num_1"])
head(community_menu)

#make a dataframe for taxicab companies and their abbreviations
company_names <- data.frame(c('Blue Ribbon Taxi Association Inc.', 
                              'Taxi Affiliation Services',
                              'Taxicab Insurance Agency, LLC', 
                              'Choice Taxi Association',
                              'Star North Management LLC', 
                              'Top Cab Affiliation',
                              'Chicago Independents', 
                              'KOAM Taxi Association',
                              '1085 - 72312 N and W Cab Co', 
                              'Chicago Medallion Management',
                              'Chicago Carriage Cab Corp', 
                              'Flash Cab', 'Globe Taxi',
                              'Patriot Taxi Dba Peace Taxi Associat', 
                              'City Service',
                              '24 Seven Taxi', 
                              'Sun Taxi', 
                              'Medallion Leasin',
                              'Taxi Affiliation Service Yellow', 
                              'Nova Taxi Affiliation Llc',
                              'Gold Coast Taxi', 
                              'Chicago Taxicab', 
                              'Blue Diamond', 
                              'Yellow Cab',
                              '312 Medallion Management Corp', 
                              'Checker Taxi Affiliation',
                              '5 Star Taxi', 
                              'Metro Jet Taxi A', 
                              'Checker Taxi',
                              '6742 - 83735 Tasha ride inc', 
                              'Setare Inc',
                              'American United Taxi Affiliation', 
                              '1469 - 64126 Omar Jada',
                              'American United',
                              '6743 - 78771 Luhak Corp', 
                              'Leonard Cab Co',
                              '4053 - 40193 Adwar H. Nikola',
                              '3011 - 66308 JBL Cab Inc.',
                              '4623 - 27290 Jay Kim', 
                              '3094 - 24059 G.L.B. Cab Co',
                              '2092 - 61288 Sbeih company', 
                              '2733 - 74600 Benny Jona',
                              '6574 - Babylon Express Inc.',
                              '3623 - 72222 Arrington Enterprises', 
                              'Chicago Star Taxicab',
                              '3721 - Santamaria Express', 
                              'Alvaro Santamaria',
                              '5006 - 39261 Salifu Bawa', 
                              '5062 - 34841 Sam Mestas',
                              '5074 - 54002 Ahzmi Inc', 
                              '3620 - 52292 David K. Cab Corp.',
                              '5874 - 73628 Sergey Cab Corp.',
                              '3591 - 63480 Chuks Cab',
                              'Petani Cab Corp',
                              'U Taxicab', 
                              '3556 - 36214 RC Andrews Cab'),
                            c('BRTAI', 
                              'TAS',
                              'TIAL', 
                              'CTA',
                              'SNML', 
                              'TCA',
                              'CI', 
                              'KTA',
                              'NWCC', 
                              'CMM',
                              'CCCC', 
                              'FC', 
                              'GT',
                              'PTDPTA', 
                              'CS',
                              '24ST', 
                              'ST', 
                              'ML',
                              'TASY', 
                              'NTAL',
                              'GCT', 
                              'CT', 
                              'BD', 
                              'YC',
                              '312MMC', 
                              'ChTA',
                              '5ST', 
                              'MJTA', 
                              'ChT',
                              '68TRI', 
                              'SI',
                              'AUTA', 
                              '16OJ',
                              'AU',
                              '67LC', 
                              'LCC',
                              '44AHN',
                              '36JCI',
                              '42JK', 
                              '32GCC',
                              '26SC', 
                              '27BJ',
                              '6BEI',
                              '37AE', 
                              'CST',
                              '3SE', 
                              'AS',
                              '53SB', 
                              '53SM',
                              '55AI', 
                              '35DKCC',
                              '57SCC',
                              '36CC',
                              'PCC',
                              'UT', 
                              '33RCAC'))
names(company_names) <- c("company", "CompanyNew")
head(company_names)


#convert from 24hr to 12hr am/pm
taxi_info$TimeNew <- format(strptime(taxi_info$Trip_Time, '%H'), '%I %p')
head(taxi_info)
str(taxi_info)
taxi_info[2000:2025,]
df_new<- setNames(count(taxi_info$Dropoff_Community_Area), c("area_num_1", "Rides"))
df_new<-merge(df_new, community_menu, by = "area_num_1")
head(df_new)
sums <- sum(as.numeric(df_new$Rides))
sums


m <- ggplot(df_new, aes(x=community, y = (Rides/sums)*100)) + 
  geom_bar(stat="identity", width=0.7, fill="#33647A") + 
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Trip Start Time", y ="Rides")+
  theme_bw() +
  theme(text = element_text(family = "sans", face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))
m




col <- c("#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6")

time_twelve <- factor(taxi_info$TimeNew, level = c('12 AM', '01 AM', '02 AM', '03 AM', '04 AM', '05 AM', '06 AM', '07 AM', '08 AM', '09 AM', '10 AM', '11 AM', '12 PM', '01 PM', '02 PM', '03 PM', '04 PM', '05 PM', '06 PM', '07 PM', '08 PM', '09 PM', '10 PM', '11 PM'))
#change plot based on community area
m <- ggplot(taxi_info, aes(x=Trip_Time)) + 
  geom_bar(stat="count", width=0.7, fill="#33647A") + 
  scale_x_continuous(breaks=seq(0,23,1)) +
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Trip Start Time", y ="Rides")+
  theme_bw() +
  theme(text = element_text(family = "sans", face = "bold")) +
  theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))
m
m
m <- ggplot(taxi_info, aes(x=Trip_Seconds)) + geom_histogram(bins=15) + geom_bar(width = 0.9)
m

#test map:
nycounties <- rgdal::readOGR("https://rstudio.github.io/leaflet/json/nycounties.geojson")

pal <- colorNumeric("viridis", NULL)

leaflet(nycounties) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5)
  
  # addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
  #             fillColor = ~pal(log10(pop)),
  #             label = ~paste0(county, ": ", formatC(pop, big.mark = ","))) %>%
  # addLegend(pal = pal, values = ~log10(pop), opacity = 1.0,
  #           labFormat = labelFormat(transform = function(x) round(10^x)))

leaflet(chi_map) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5) %>%
  addLegend(pal = pal, values = c(10,20,30), opacity = 1.0)


#testing selecting specific area
data_new <- subset(taxi_info,  Pickup_Community_Area == 2)
targetCol <- "Dropoff_Community_Area"

print(subset(data_new, select = "Dropoff_Community_Area"))
print(data_new[data_new$Dropoff_Community_Area == 52, ])

totalRidesHere <- nrow(data_new)
print(totalRidesHere)
defaultCA <- data.frame(1:77, 0)
names(defaultCA) <- c("Dropoff_Community_Area", "freq")
head(defaultCA, 80)

countsPerArea <- count(data_new, targetCol)

newDF <- data.frame()

for(x in 1:77) {
  if (x %in% countsPerArea) {
    newDF[nrow(newDF)+1,] = c(x, countsPerArea)
  }
}

head(countsPerArea$Dropoff_Community_Area, countsPerArea$freq, 80)
#countsPerArea <- count(data_new, targetCol)
head(countsPerArea,80)
countsPerArea$Percent <- (countsPerArea$freq / totalRidesHere)*100
head(data_new)
#using table to count:
countsArea <- as.data.frame(group_by(data_new, Dropoff_Community_Area) %>%
              dplyr::summarise(freq=n()) %>%
  ungroup() %>%
  tidyr::complete(Dropoff_Community_Area,
           fill = list(N = 0)))
#countsArea <- data.frame(table(data_new$Dropoff_Community_Area))
head(countsArea, 80)

binpal <- colorBin("YlOrRd", countsPerArea$Percent, 8)

leaflet(chi_map) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8, fillColor = ~binpal(countsPerArea$Percent)) %>%
  addLegend(pal = binpal, values = countsPerArea$Percent, opacity = 1.0)

head(countsPerArea, 80)
