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
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("About", tabName = "About", icon = NULL),
                     menuItem("Data Visualizations", tabName = "Datavisualizations", icon = NULL, selected = TRUE))
                   ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "About",
              h1("CTA Ridership Map Project"),
              h2("Ridership Data From: Chicago Data Portal at https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
              h2("Stations Location Data From: Chicago Data Portal at https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme"),
              h2("Application Written by Ameesha Saxena and Rafiya Awan for UIC CS 424 Spring 2022")
              
              ), # tabitem About close
      tabItem(tabName = "Datavisualizations",
              tags$style(HTML("
              .box.box-solid.box-primary>.box-header {
              color:#fff;
              background:#003d59
              }
              .box.box-solid.box-primary{
              border-bottom-color:#666666;
              border-left-color:#666666;
              border-right-color:#666666;
              border-top-color:#666666;
              }
              /* body */
              .content-wrapper, .right-side {
              background-color: #f5f5f5;
              }
                              ")
              ),
              fluidRow(
                column(2,
                       align = "center",
                       #bar chart showing the distribution of the number of rides by day of year (Jan 1 through Dec 31)
                       fluidRow(
                         box(title = textOutput("RidesByDateText"), solidHeader = TRUE, status = "primary", width = 12,
                             plotOutput("RidesByDate", height = 300)
                             )
                         ),
                       #bar chart showing the distribution of the number of rides by hour of day based on start time (midnight through 11pm)
                       fluidRow(
                         box(title = textOutput("RidesByStartText"), solidHeader = TRUE, status = "primary", width = 12,
                             plotOutput("RidesByStart", height = 300)
                             )
                         ),
                       #bar chart showing the distribution of the number of rides by day of week (Monday through Sunday)
                       fluidRow(
                         box(title = textOutput("RidesByWeekdayText"), solidHeader = TRUE, status = "primary", width = 12,
                             plotOutput("RidesByWeekday", height = 300)
                             )
                         )
                       ),
                column(2,
                       align = "center",
                       #bar chart showing the distribution of the number of rides by month of year (Jan through Dec)
                       fluidRow(
                         box(title = textOutput("RidesByMonthText"), solidHeader = TRUE, status = "primary", width = 12,
                             plotOutput("RidesByMonth", height = 300)
                         )
                       ),
                       #bar chart showing the distribution of the number of rides by binned mileage (with an appropriate number of bins)
                       fluidRow(
                         box(title = textOutput("RidesByMileageText"), solidHeader = TRUE, status = "primary", width = 12,
                             plotOutput("RidesByMileage", height = 300)
                         )
                       ),
                       #bar chart showing the distribution of the number of rides by binned trip time (with an appropriate number of bins)
                       fluidRow(
                         box(title = textOutput("RidesByTimeText"), solidHeader = TRUE, status = "primary", width = 12,
                             plotOutput("RidesByTime", height = 300)
                         )
                       )
                ),
              
                
              )#end fluidrow
              
              
              ) # tabitem Visualizations close
      ) #tabitems close
    ) #dashboardBodyClose
  ) #dashboardPage
  
  
#server functions
server <- function(input, output, session) {
  
  #text return functions for box headers
  output$RidesByDateText <- renderText({
    return("Number of Rides on each day of the year")
  })
  output$RidesByStartText <- renderText({
    return(paste("Number of Rides based on start time"))
  })
  output$RidesByWeekdayText <- renderText({
    return(paste("Number of Rides on each day of the week"))
  })
  output$RidesByMonthText <- renderText({
    return(paste("Number of Rides in each month"))
  })
  output$RidesByMileageText <- renderText({
    return(paste("Number of Rides based on mileage"))
  })
  output$RidesByTimeText <- renderText({
    return(paste("Number of Rides with trip time"))
  })
  
  
  #bar chart generating functions 
  output$RidesByDate <- renderPlot({
    m <- ggplot(taxi_info, aes(x=Trip_Date)) + 
      geom_bar(stat="count", width=0.7, fill="#33647A") + 
      scale_y_continuous(labels = scales::comma)
    m
    m
  })
  
  output$RidesByStart <- renderPlot({
    m <- ggplot(taxi_info, aes(x=Trip_Time)) + 
      geom_bar(stat="count", width=0.7, fill="#33647A") + 
      scale_y_continuous(labels = scales::comma)
    m
  })
  
  output$RidesByWeekday <- renderPlot({
    m <- ggplot(taxi_info, aes(x=wday)) + 
      geom_bar(stat="count", width=0.7, fill="#33647A") + 
      scale_y_continuous(labels = scales::comma)
    m
  })
  
  output$RidesByMonth <- renderPlot({
    m <- ggplot(taxi_info, aes(x=month)) + 
      geom_bar(stat="count", width=0.7, fill="#33647A") + 
      scale_y_continuous(labels = scales::comma)
    m
  })
  output$RidesByMileage <- renderPlot({
    m <- ggplot(taxi_info, aes(x=Trip_Time)) + 
      geom_bar(stat="count", width=0.7, fill="#33647A") + 
      scale_y_continuous(labels = scales::comma)
    m
  })
  output$RidesByTime <- renderPlot({
    m <- ggplot(taxi_info, aes(x=Trip_Time)) + 
      geom_bar(stat="count", width=0.7, fill="#33647A") + 
      scale_y_continuous(labels = scales::comma)
    m
  })
  
}

shinyApp(ui = ui, server = server)


