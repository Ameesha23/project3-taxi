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
library(sf)

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

#separate a df with pickup areas and their corresponding rides
pickups_comm <- setNames(count(taxi_info$Pickup_Community_Area), c("area_num_1", "Rides"))
drop_comm <- setNames(count(taxi_info$Dropoff_Community_Area), c("area_num_1", "Rides"))

#information about the community areas
chi_map <- read_sf("https://raw.githubusercontent.com/thisisdaryn/data/master/geo/chicago/Comm_Areas.geojson") 
chi_map$area_num_1 = as.numeric(chi_map$area_num_1)
pickup_map <- left_join(chi_map, pickups_comm, by = ("area_num_1"))
dropoff_map <- left_join(chi_map, drop_comm, by = ("area_num_1"))

#make a menu for displaying the menu for selecting community areas
community_menu <- data.frame(chi_map$community, chi_map$area_num_1)
names(community_menu) <- c("community", "area_num_1")
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

