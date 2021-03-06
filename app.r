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
#library(measurements)
library(sf)
#remotes::install_github("willdebras/shinykeyboard")

#display five digits for all numbers 
options(digits = 5)              

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

#add column with distance in km - convert trip miles to km
taxi_info$Trip_km = taxi_info$Trip_Miles * 1.60934

#add column with start time as 12hr am/pm
taxi_info$Time_Twelve <- format(strptime(taxi_info$Trip_Time, '%H'), '%I %p')

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
community_menu <- community_menu[order(community_menu$community),]
new_row <- c('City of Chicago','0')
community_menu <- rbind(new_row, community_menu)   

#re-order shapefile features so community areas are in order
chi_map <- chi_map[order(chi_map$area_num_1),]

targetCol <- "Dropoff_Community_Area"

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
company_names <- company_names[order(company_names$company),]
new_row <- c('All Taxi Companies','ALL')
company_names <- rbind(new_row, company_names)   

print(head(taxi_info))
print(head(community_menu))
print(head(company_names))

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
                         radioButtons("selectionFilter", h4("Filter by Community Area or by Cab Company:"),
                                     choices = list("Community Area" = 0,
                                                    "Company" = 1)),
                         HTML("<br>"),
                         
                         conditionalPanel(
                           condition = "input.selectionFilter == 0",
                           fluidRow(
                             selectInput("comm_area", "Select Community Area", 
                                         community_menu$community, selected = 'City of Chicago'),
                             radioButtons("direction", "Rides FROM or TO the Community Area",
                                          choices = list("From" = 0, 
                                                         "To" = 1),selected = 0),
                           ),
                         ),
                         conditionalPanel(
                           condition = "input.selectionFilter == 1",
                           fluidRow(
                             selectInput("company", "Select Taxicab Company", 
                                         company_names$company, selected = 'All Taxi Companies'),
                             radioButtons("direction2", "Rides Given FROM or TO Each Community Area By Taxi Company",
                                          choices = list("From" = 0, 
                                                         "To" = 1),selected = 0),
                           )
                         )
                         
                         #TODO add virtual keyboard
                         #https://github.com/Emelieh21/shinykeyboard
                         ),
                       ),
                column(8,
                       fluidRow(
                         conditionalPanel(
                           condition = "input.viewTables == 0",
                           fluidRow(
                                 #bar chart showing the distribution of the number of rides by day of year (Jan 1 through Dec 31)
                                 column(4,
                                 fluidRow(
                                   style = "padding-left:20px",
                                   box(title = textOutput("RidesByDateText"), solidHeader = TRUE, status = "primary", width = 12,
                                       plotOutput("RidesByDate", height = 350)
                                   )
                                 )
                                 ),
                                 #bar chart showing the distribution of the number of rides by hour of day based on start time (midnight through 11pm)
                                 column(4,
                                 fluidRow(
                                   style = "padding-left:20px",
                                   box(title = textOutput("RidesByStartText"), solidHeader = TRUE, status = "primary", width = 12,
                                       plotOutput("RidesByStart", height = 350)
                                   )
                                 )
                                 ),
                                 #bar chart showing the distribution of the number of rides by day of week (Monday through Sunday)
                                 column(4,
                                 fluidRow(
                                   style = "padding-left:20px",
                                   box(title = textOutput("RidesByWeekdayText"), solidHeader = TRUE, status = "primary", width = 12,
                                       plotOutput("RidesByWeekday", height = 350)
                                   )
                                 )
                                 )
                           ),
                           fluidRow(
                                  #bar chart showing the distribution of the number of rides by month of year (Jan through Dec)
                                  column(4,
                                  fluidRow(
                                    style = "padding-left:20px",
                                    box(title = textOutput("RidesByMonthText"), solidHeader = TRUE, status = "primary", width = 12,
                                        plotOutput("RidesByMonth", height = 350)
                                    )
                                  )
                                  ),
                                  #bar chart showing the distribution of the number of rides by binned mileage (with an appropriate number of bins)
                                  column(4,
                                  fluidRow(
                                    style = "padding-left:20px",
                                    box(title = textOutput("RidesByMileageText"), solidHeader = TRUE, status = "primary", width = 12,
                                        plotOutput("RidesByMileage", height = 350)
                                    )
                                  )
                                  ),
                                  #bar chart showing the distribution of the number of rides by binned trip time (with an appropriate number of bins)
                                  column(4,
                                  fluidRow(
                                    style = "padding-left:20px",
                                    box(title = textOutput("RidesByTimeText"), solidHeader = TRUE, status = "primary", width = 12,
                                        plotOutput("RidesByTime", height = 350)
                                    )
                                  )
                                  )
                           ),
                           fluidRow(
                             column(2),
                             column(8,
                                    fluidRow(
                                      style = "padding-left:20px",
                                      box(title = textOutput("RidesForCommArea"), solidHeader = TRUE, status = "primary", width = 12,
                                          plotOutput("RidesByCommArea", height = 350)
                                      )
                                    )
                             ),
                             column(2)
                           )
                         ), #end of first, plots, conditionalPanel
                         conditionalPanel(
                           condition = "input.viewTables == 1",
                           fluidRow(
                                  #table showing the distribution of the number of rides by day of year (Jan 1 through Dec 31)
                                  column(4,
                                  fluidRow(
                                    box(title = textOutput("RidesByDateText2"), solidHeader = TRUE, status = "primary", width = 12,
                                        div(DT::dataTableOutput("TableByDate", height = 350), style = "font-size:100%")
                                    )
                                  )
                                  ),
                                  #table showing the distribution of the number of rides by hour of day based on start time (midnight through 11pm)
                                  column(4,
                                  fluidRow(
                                    box(title = textOutput("RidesByStartText2"), solidHeader = TRUE, status = "primary", width = 12,
                                        div(DT::dataTableOutput("TableByStart", height = 350), style = "font-size:100%")
                                    )
                                  )
                                  ),
                                  #table showing the distribution of the number of rides by day of week (Monday through Sunday)
                                  column(4,
                                  fluidRow(
                                    box(title = textOutput("RidesByWeekdayText2"), solidHeader = TRUE, status = "primary", width = 12,
                                        div(DT::dataTableOutput("TableByWeekday", height = 350), style = "font-size:100%")
                                    )
                                  )
                                  )
                           ),
                           fluidRow(
                                  #table showing the distribution of the number of rides by month of year (Jan through Dec)
                                  column(4,
                                  fluidRow(
                                    box(title = textOutput("RidesByMonthText2"), solidHeader = TRUE, status = "primary", width = 12,
                                        div(DT::dataTableOutput("TableByMonth", height = 350), style = "font-size:100%")
                                    )
                                  )
                                  ),
                                  #table showing the distribution of the number of rides by binned mileage (with an appropriate number of bins)
                                  column(4,
                                  fluidRow(
                                    box(title = textOutput("RidesByMileageText2"), solidHeader = TRUE, status = "primary", width = 12,
                                        div(DT::dataTableOutput("TableByMileage", height = 350), style = "font-size:100%")
                                    )
                                  )
                                  ),
                                  #table showing the distribution of the number of rides by binned trip time (with an appropriate number of bins)
                                  column(4,
                                  fluidRow(
                                    box(title = textOutput("RidesByTimeText2"), solidHeader = TRUE, status = "primary", width = 12,
                                        div(DT::dataTableOutput("TableByTime", height = 350), style = "font-size:100%")
                                    )
                                  )
                                  )
                           )
                         ) # end of second, tables, conditionalPanel
                       )
                ),
                column(3,
                       align="center",
                       fluidRow(
                         style = "padding-left:20px",
                         box(title = "Map of Community Areas", solidHeader = TRUE, status = "primary", width = 12,
                             leafletOutput("commMap", height = 1200)
                         )
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
  
  comm_area<-reactive({
    input$comm_area
  })
  
  direction<-reactive({
    input$direction
  })
  
  direction2<-reactive({
    input$direction2
  })
  
  company<-reactive({
    input$company
  })
  
  filterType<-reactive({
    input$selectionFilter
  })
  
  
  # change company input based on community area selection
  observeEvent(input$comm_area, {
    
    if(comm_area() != 'City of Chicago') {
        updateSelectInput(session, "company", "Select Taxicab Company", company_names$company, selected = 'All Taxi Companies')
    }
    
  })
  
  # change community area input based on company selection
  observeEvent(input$company, {
    
    if(company() != 'All Taxi Companies') {
        updateSelectInput(session, "comm_area", "Select Community Area", community_menu$community, selected = 'City of Chicago')
    }
    
  })

  #register map click and update selectInput
  observeEvent(input$commMap_shape_click, {
    click <- input$commMap_shape_click
    #print(click)
    updateSelectInput(session, "comm_area", "Select Community Area", community_menu$community, selected = click$id)
    updateRadioButtons(session, "selectionFilter", "Filter by Community Area or by Cab Company:",
                 choices = list("Community Area" = 0,
                                "Company" = 1), selected = 0)

    #print(comm_area())
  })

  #TODO change to independant
  data_new<-reactive({
    if(comm_area() == 'City of Chicago' || company() == 'All Taxi Companies'){
      data_new <- taxi_info
    }
    if(comm_area() != 'City of Chicago' && direction() == 0){
      data_new <- subset(taxi_info,  Pickup_Community_Area == community_menu[which(community_menu$community == comm_area()), "area_num_1"])
      targetCol <- "Dropoff_Community_Area"
    }
    if(comm_area() != 'City of Chicago' && direction() == 1){
      data_new <- subset(taxi_info,  Dropoff_Community_Area == community_menu[which(community_menu$community == comm_area()), "area_num_1"])
      targetCol <- "Pickup_Community_Area"
    }
    if(company() != 'All Taxi Companies'){
      data_new <- subset(taxi_info,  CompanyNew == company_names[which(company_names$company == company()), "CompanyNew"])
    }
    data_new
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
    return(paste("Number of Rides By Distance Traveled"))
  })
  output$RidesByTimeText <- renderText({
    return(paste("Number of Rides By Trip Time"))
  })
  output$RidesForCommArea <- renderText({
    return(paste("Percent of Rides To/From Selected Community Area"))
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
    return(paste("Number of Rides By Distance Traveled"))
  })
  output$RidesByTimeText2 <- renderText({
    return(paste("Number of Rides By Trip Time"))
  })
  
  
  #bar chart generating functions 
  #figure out how to add all communities
  output$RidesByDate <- renderPlot({
    col <- c("#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6", "#3e6a7f", "#749aa6")
    
    
    #change plot based on community area
    m <- ggplot(data_new(), aes(x=Trip_Date, fill = month)) + 
      geom_bar(stat="count", width=0.7, show.legend = FALSE) + 
      scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
      scale_x_date(date_breaks = "1 month", date_labels = "%B") +
      labs(x = "Trip Date", y ="Rides") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))+
      scale_fill_manual(values = col)
    m
  })
  
  output$RidesByStart <- renderPlot({
    
    #check if user wants time in 12 hour or 24 hour format
    if(timeAs() == 0) {
      time_twelve <- factor(data_new()$Time_Twelve, level = c('12 AM', '01 AM', '02 AM', '03 AM', '04 AM', '05 AM', '06 AM', '07 AM', '08 AM', '09 AM', '10 AM', '11 AM', '12 PM', '01 PM', '02 PM', '03 PM', '04 PM', '05 PM', '06 PM', '07 PM', '08 PM', '09 PM', '10 PM', '11 PM'))
      m <- ggplot(data_new(), aes(x=time_twelve)) + 
        geom_bar(stat="count", width=0.7, fill="#33647A") + 
        scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
        labs(x = "Trip Start Time", y ="Rides")
    }
    else {
      m <- ggplot(data_new(), aes(x=Trip_Time)) + 
        scale_x_continuous(breaks=seq(0,23,1)) +
        geom_bar(stat="count", width=0.7, fill="#33647A", breaks = scales::pretty_breaks(n = 10)) + 
        scale_y_continuous(labels = scales::comma, ) +
        labs(x = "Trip Start Time", y ="Rides")
    }
    
    m <- m + theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))
    
    
    m
  })
  
  output$RidesByWeekday <- renderPlot({
    m <- ggplot(data_new(), aes(x=wday)) + 
      geom_bar(stat="count", width=0.7, fill="#33647A") + 
      scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
      labs(x = "Weekday", y ="Rides") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))
    m
  })
  
  output$RidesByMonth <- renderPlot({
    m <- ggplot(data_new(), aes(x=month)) + 
      geom_bar(stat="count", width=0.7, fill="#33647A") + 
      scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
      labs(x = "Month", y ="Rides") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))
    m
  })
  
  output$RidesByMileage <- renderPlot({
    
    #TODO: add space between bars
    #TODO logarithmic binning
    
    #check if user wants distance in mi or km
    if(miles() == 0) {
      m <- ggplot(data_new(), aes(x=Trip_Miles, width=0.75)) + 
        geom_bar(stat="bin", binwidth = 5, fill="#33647A") + 
        scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
        labs(x = "Trip Distance (Miles)", y ="Rides")
    }
    else {
      m <- ggplot(data_new(), aes(x=Trip_km, width=0.75)) + 
        geom_bar(stat="bin", binwidth = 5, fill="#33647A") + 
        scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
        labs(x = "Trip Distance (Kilometers)", y ="Rides")
    }
    
    m <- m + theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))
    
    
    m
  })
  
  #can't figure out how to add space between bars TODO
  #TODO logarithmic binning
  output$RidesByTime <- renderPlot({
    # TODO: add space between bars + find better division of bins
    m <- ggplot(data_new(), aes(x=Trip_Seconds, width=0.75)) + 
      geom_bar(stat="bin", binwidth = 300, fill="#33647A") + 
      scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
      labs(x = "Total Trip Time (Seconds)", y ="Rides") + 
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12))
    m
    
  })
  
  
  
  output$RidesByCommArea <- renderPlot({
    #TODO - Add plot for percent of rides to/from each community area
    #percentage of rides to each community area from selected community area
    
    if (filterType() == 0) {
      if(direction() == 0){
        #count the dropoffs in other community areas
        df_new<- setNames(count(data_new()$Dropoff_Community_Area), c("area_num_1", "Rides"))
      }
      #percentage of rides from each community area to selected community area
      if(direction() == 1){
        df_new<- setNames(count(data_new()$Pickup_Community_Area), c("area_num_1", "Rides"))
      }  
    }
    else if (filterType() == 1) {
      if (company() != 'All Taxi Companies') {
        if(direction2() == 0){
          #count the dropoffs in other community areas
          df_new<- setNames(count(data_new()$Pickup_Community_Area), c("area_num_1", "Rides"))
        }
        #percentage of rides from each community area to selected community area
        if(direction2() == 1){
          df_new<- setNames(count(data_new()$Dropoff_Community_Area), c("area_num_1", "Rides"))
        }    
      }
      else {
        if(direction2() == 0){
          #count the dropoffs in other community areas
          df_new<- setNames(count(data_new()$Dropoff_Community_Area), c("area_num_1", "Rides"))
        }
        #percentage of rides from each community area to selected community area
        if(direction2() == 1){
          df_new<- setNames(count(data_new()$Pickup_Community_Area), c("area_num_1", "Rides"))
        }
      }
      
    }
    
    #add in missing data with 0 Rides
    for(x in 1:77) {
      if (!(x %in% df_new$area_num_1)) {
        df_new<-rbind(df_new, data.frame(area_num_1=x,Rides=0))
      }
    }
    
    #order all areas from 1-77
    df_new <- df_new[order(df_new$area_num_1),]
    
    df_new<-merge(df_new, community_menu, by = "area_num_1")
    sums <- sum(as.numeric(df_new$Rides))
    m <- ggplot(df_new, aes(x=community, y = (Rides/sums)*100)) + 
      geom_bar(stat="identity", width=0.7, fill="#33647A") + 
      geom_text(aes(label = sprintf("%0.2f", round((Rides/sums)*100, digits = 2))), vjust = -0.2) +
      scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
      labs(x = "Community Area", y ="Rides")+
      theme_bw() +
      theme(text = element_text(family = "sans", face = "bold")) +
      theme(plot.title = element_text(hjust = 0.5, size=20), axis.title=element_text(size=12), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    m
  })
  
  output$commMap <- renderLeaflet({
    marker_color = "#33647A"
    
    selectedData <- data_new()
    totalRidesHere <- nrow(selectedData)
    
    #calculate total rides for each area
    
    if (filterType() == 0 ) {
      if (direction() == 0) {
        countsPerArea <- count(selectedData, "Dropoff_Community_Area")  
      }
      
      if (direction() == 1) {
        countsPerArea <- count(selectedData, "Pickup_Community_Area")  
      }
    }
    else if (filterType() == 1) {
      if (company() != 'All Taxi Companies') {
        if (direction2() == 0) {
          countsPerArea <- count(selectedData, "Pickup_Community_Area")  
        }
        
        if (direction2() == 1) {
          countsPerArea <- count(selectedData, "Dropoff_Community_Area")  
        }  
      }
      else {
        if (direction2() == 0) {
          countsPerArea <- count(selectedData, "Dropoff_Community_Area")  
        }
        
        if (direction2() == 1) {
          countsPerArea <- count(selectedData, "Pickup_Community_Area")  
        }
      }
      
    }
    
    names(countsPerArea)[1] <- 'area'
    
    #add in missing data
    for(x in 1:77) {
      if (!(x %in% countsPerArea$area)) {
        countsPerArea<-rbind(countsPerArea, data.frame(area=x,freq=0))
      }
    }
    
    #order all areas from 1-77
    countsPerArea <- countsPerArea[order(countsPerArea$area),]
    #calculate and add percent of rides
    countsPerArea$Percent <- (countsPerArea$freq / totalRidesHere)*100
    
    binpal <- colorBin("PuBu", countsPerArea$Percent, 6)
    
  
    leaflet(chi_map) %>%
      addTiles() %>%
      #addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.8, fillColor = ~binpal(countsPerArea$Percent), label = ~community,
                  highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE), layerId = ~community) %>%
      addLegend(pal = binpal, values = countsPerArea$Percent, title = "Percentage of Rides for Each Area", labFormat = labelFormat(suffix = "%"), opacity = 1.0)
    
    
  })
  
  output$TableByDate <- DT::renderDataTable(
    DT::datatable({ 
      df_new<- setNames(count(data_new()$TripDate), c("Date", "Rides"))
      df_new
    }, 
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableByStart <- DT::renderDataTable(
    DT::datatable({
      #check if user wants time in 12 hour or 24 hour format #TODO: change order according to AM/PM
      if(timeAs() == 0) {
        df_new <- setNames(count(data_new()$Time_Twelve), c("Start Time", "Rides"))
      }
      else {
        df_new <- setNames(count(data_new()$Trip_Time), c("Start Time", "Rides"))
      }
      
      df_new
    }, 
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableByWeekday <- DT::renderDataTable(
    DT::datatable({ 
      df_new<- setNames(count(data_new()$wday), c("Day", "Rides"))
      df_new
    }, 
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableByMonth <- DT::renderDataTable(
    DT::datatable({ 
      df_new<- setNames(count(data_new()$month), c("Month", "Rides"))
      df_new
    }, 
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableByMileage <- DT::renderDataTable(
    DT::datatable({
      #check if user wants distance in mi or km
      if(miles() == 0) {
        df_new<- setNames(count(data_new()$Trip_Miles), c("Trip Miles", "Rides"))
      }
      else {
        df_new<- setNames(count(data_new()$Trip_km), c("Trip Kilometers", "Rides"))
      }
      df_new
    }, 
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  output$TableByTime <- DT::renderDataTable(
    DT::datatable({ 
      df_new<- setNames(count(data_new()$Trip_Seconds), c("Trip Time (Seconds)", "Rides"))
      df_new
    }, 
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
}

shinyApp(ui = ui, server = server)

