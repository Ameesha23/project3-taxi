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
#taxi_info$TripDate = as_date(ymd(taxi_info$Trip_Date))

#add day, month and year data to csv
#taxi_info$year = year(taxi_info$TripDate)
#taxi_info$month = month(taxi_info$TripDate, abbr = TRUE, label = TRUE)
#taxi_info$wday = wday(taxi_info$TripDate, label=TRUE)

print(head(taxi_info))

#create the ui
ui <- dashboardPage(
  #change header color
  skin = "black",
  
  dashboardHeader(title = "Big Yellow Taxi"),
  dashboardSidebar(disable = FALSE, collapsed = TRUE,
                   
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL))
                     #menuItem("About", tabName = "About", icon = NULL),
                     #menuItem("Data Visualizations", tabName = "Datavisualizations", icon = NULL, selected = TRUE))
                   ),
  dashboardBody(
  )
  )

#server functions
server <- function(input, output, session) {
  
}

shinyApp(ui = ui, server = server)


