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
              h1("Taxi Ridership Map Project"),
              h2("2019 Taxi Data From: Chicago Data Portal at https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy"),
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
                column(1,
                       align = "center",
                       fluidRow(
                         style = "padding-left:20px",
                         HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>"),
                         h3("Options"),
                         radioButtons("viewTables", h4("View Data As:"),
                                      choices = list("Charts" = 0, 
                                                     "Tables" = 1),selected = 0),
                         radioButtons("miles", h4("Change Distance Unit"),
                                      choices = list("Miles" = 0, 
                                                     "Kilometers" = 1),selected = 0),
                         radioButtons("timeAs", h4("View Time As:"),
                                      choices = list("12Hr" = 0, 
                                                     "24Hr" = 1),selected = 0),
                         )
                       ),
                column(4,
                       fluidRow(
                         
                         conditionalPanel(
                           condition = "input.viewTables == 0",
                           column(6,
                                 #bar chart showing the distribution of the number of rides by day of year (Jan 1 through Dec 31)
                                 fluidRow(
                                   style = "padding-left:20px",
                                   box(title = textOutput("RidesByDateText"), solidHeader = TRUE, status = "primary", width = 12,
                                       plotOutput("RidesByDate", height = 300)
                                   )
                                 ),
                                 #bar chart showing the distribution of the number of rides by hour of day based on start time (midnight through 11pm)
                                 fluidRow(
                                   style = "padding-left:20px",
                                   box(title = textOutput("RidesByStartText"), solidHeader = TRUE, status = "primary", width = 12,
                                       plotOutput("RidesByStart", height = 300)
                                   )
                                 ),
                                 #bar chart showing the distribution of the number of rides by day of week (Monday through Sunday)
                                 fluidRow(
                                   style = "padding-left:20px",
                                   box(title = textOutput("RidesByWeekdayText"), solidHeader = TRUE, status = "primary", width = 12,
                                       plotOutput("RidesByWeekday", height = 300)
                                   )
                                 )
                           ),
                           column(6,
                                  #bar chart showing the distribution of the number of rides by month of year (Jan through Dec)
                                  fluidRow(
                                    style = "padding-left:20px",
                                    box(title = textOutput("RidesByMonthText"), solidHeader = TRUE, status = "primary", width = 12,
                                        plotOutput("RidesByMonth", height = 300)
                                    )
                                  ),
                                  #bar chart showing the distribution of the number of rides by binned mileage (with an appropriate number of bins)
                                  fluidRow(
                                    style = "padding-left:20px",
                                    box(title = textOutput("RidesByMileageText"), solidHeader = TRUE, status = "primary", width = 12,
                                        plotOutput("RidesByMileage", height = 300)
                                    )
                                  ),
                                  #bar chart showing the distribution of the number of rides by binned trip time (with an appropriate number of bins)
                                  fluidRow(
                                    style = "padding-left:20px",
                                    box(title = textOutput("RidesByTimeText"), solidHeader = TRUE, status = "primary", width = 12,
                                        plotOutput("RidesByTime", height = 300)
                                    )
                                  )
                           )
                         ), #end of first, plots, conditionalPanel
                         conditionalPanel(
                           condition = "input.viewTables == 1",
                           column(6,
                                  #table showing the distribution of the number of rides by day of year (Jan 1 through Dec 31)
                                  fluidRow(
                                    box(title = textOutput("RidesByDateText2"), solidHeader = TRUE, status = "primary", width = 12,
                                        div(DT::dataTableOutput("TableByDate"), style = "font-size:100%")
                                    )
                                  ),
                                  #table showing the distribution of the number of rides by hour of day based on start time (midnight through 11pm)
                                  fluidRow(
                                    box(title = textOutput("RidesByStartText2"), solidHeader = TRUE, status = "primary", width = 12,
                                        div(DT::dataTableOutput("TableByStart"), style = "font-size:100%")
                                    )
                                  ),
                                  #table showing the distribution of the number of rides by day of week (Monday through Sunday)
                                  fluidRow(
                                    box(title = textOutput("RidesByWeekdayText2"), solidHeader = TRUE, status = "primary", width = 12,
                                        div(DT::dataTableOutput("TableByWeekday"), style = "font-size:100%")
                                    )
                                  )
                           ),
                           column(6,
                                  #table showing the distribution of the number of rides by month of year (Jan through Dec)
                                  fluidRow(
                                    box(title = textOutput("RidesByMonthText2"), solidHeader = TRUE, status = "primary", width = 12,
                                        div(DT::dataTableOutput("TableByMonth"), style = "font-size:100%")
                                    )
                                  ),
                                  #table showing the distribution of the number of rides by binned mileage (with an appropriate number of bins)
                                  fluidRow(
                                    box(title = textOutput("RidesByMileageText2"), solidHeader = TRUE, status = "primary", width = 12,
                                        div(DT::dataTableOutput("TableByMileage"), style = "font-size:100%")
                                    )
                                  ),
                                  #table showing the distribution of the number of rides by binned trip time (with an appropriate number of bins)
                                  fluidRow(
                                    box(title = textOutput("RidesByTimeText2"), solidHeader = TRUE, status = "primary", width = 12,
                                        div(DT::dataTableOutput("TableByTime"), style = "font-size:100%")
                                    )
                                  )
                           )
                         ) # end of second, tables, conditionalPanel
                       )
                )
                
              )#end fluidrow
              
              
      ) # tabitem Visualizations close
    ) #tabitems close
  ) #dashboardBodyClose
) #dashboardPage


#server functions
server <- function(input, output, session) {
  
  # inputs
  
  viewTables<- reactive({
    input$viewTables
  })
  
  miles<-reactive({
    input$miles
  })
  
  timeAs<-reactive({
    input$timeAs
  })
  
  
  #text return functions for box plot headers
  output$RidesByDateText <- renderText({
    return("Number of Rides By Each Day of the Year")
  })
  output$RidesByStartText <- renderText({
    return(paste("Number of Rides By Hour of Day (Start Time)"))
  })
  output$RidesByWeekdayText <- renderText({
    return(paste("Number of Rides By Each Day of the Week"))
  })
  output$RidesByMonthText <- renderText({
    return(paste("Number of Rides By Month"))
  })
  output$RidesByMileageText <- renderText({
    return(paste("Number of Rides By Mileage"))
  })
  output$RidesByTimeText <- renderText({
    return(paste("Number of Rides By Trip Time"))
  })
  
  #text return functions for box table headers
  output$RidesByDateText2 <- renderText({
    return("Number of Rides By Each Day of the Year")
  })
  output$RidesByStartText2 <- renderText({
    return(paste("Number of Rides By Hour of Day (Start Time)"))
  })
  output$RidesByWeekdayText2 <- renderText({
    return(paste("Number of Rides By Each Day of the Week"))
  })
  output$RidesByMonthText2 <- renderText({
    return(paste("Number of Rides By Month"))
  })
  output$RidesByMileageText2 <- renderText({
    return(paste("Number of Rides By Mileage"))
  })
  output$RidesByTimeText2 <- renderText({
    return(paste("Number of Rides By Trip Time"))
  })
  
  
  #bar chart generating functions 
  output$RidesByDate <- renderPlot({
    col <- c("#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6")
    
    m <- ggplot(taxi_info, aes(x=Trip_Date, fill = month(Trip_Date, abbr = TRUE, label = TRUE))) + 
      geom_bar(stat="count", width=0.7, fill="#33647A") + 
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Trip Date", y ="Rides") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))  
    m
  })
  
  output$RidesByStart <- renderPlot({
    m <- ggplot(taxi_info, aes(x=Trip_Time)) + 
      geom_bar(stat="count", width=0.7, fill="#33647A") + 
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Trip Start Time", y ="Rides") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))
    m
  })
  
  output$RidesByWeekday <- renderPlot({
    m <- ggplot(taxi_info, aes(x=wday)) + 
      geom_bar(stat="count", width=0.7, fill="#33647A") + 
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Weekday", y ="Rides") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))
    m
  })
  
  output$RidesByMonth <- renderPlot({
    m <- ggplot(taxi_info, aes(x=month)) + 
      geom_bar(stat="count", width=0.7, fill="#33647A") + 
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Month", y ="Rides") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))
    m
  })
  output$RidesByMileage <- renderPlot({
    
    #TODO: add space between bars
    m <- ggplot(taxi_info, aes(x=Trip_Miles)) + 
      geom_bar(stat="bin", binwidth = 5, fill="#33647A") + 
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Trip Miles", y ="Rides") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))
    m
  })
  
  output$RidesByTime <- renderPlot({
    # TODO: add space between bars + find better division of bins
    m <- ggplot(taxi_info, aes(x=Trip_Seconds)) + 
      geom_bar(stat="bin", binwidth = 5, fill="#33647A") + 
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Total Trip Time", y ="Rides") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))
    m
    
  })
  
  output$TableByDate <- DT::renderDataTable(
    DT::datatable({ 
      df_new<- setNames(count(taxi_info$TripDate), c("Date", "Rides"))
      df_new
    }, 
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableByStart <- DT::renderDataTable(
    DT::datatable({ 
      df_new<- setNames(count(taxi_info$Trip_Time), c("Start Time", "Rides"))
      df_new
    }, 
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableByWeekday <- DT::renderDataTable(
    DT::datatable({ 
      df_new<- setNames(count(taxi_info$wday), c("Day", "Rides"))
      df_new
    }, 
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableByMonth <- DT::renderDataTable(
    DT::datatable({ 
      df_new<- setNames(count(taxi_info$month), c("Month", "Rides"))
      df_new
    }, 
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableByMileage <- DT::renderDataTable(
    DT::datatable({ 
      df_new<- setNames(count(taxi_info$Trip_Miles), c("Trip Miles", "Rides"))
      df_new
    }, 
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableByTime <- DT::renderDataTable(
    DT::datatable({ 
      df_new<- setNames(count(taxi_info$Trip_Seconds), c("Trip Time", "Rides"))
      df_new
    }, 
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
}

shinyApp(ui = ui, server = server)

