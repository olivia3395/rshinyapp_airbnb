
#### please set your word directory first!
#setwd("/Users/qinz/Documents/R/5243/project2/airbnb")

library(geosphere)  ## for calculating the distance between lng, lat
library(dplyr)
library(shiny)
library(tidyr)
library(shinydashboard)
library(leaflet)
library(shinyWidgets)
library(stringr)
library(ggplot2)
library(lubridate)
library(plotly)    ## plotly
library(packrat)   ## for publish shiny
library(rsconnect) ## for publish shiny


#### read data
load(file = "tab1_data.RData")   ## tab1
airbnb = read.csv("AB_NYC_2019.csv")
airbnb = airbnb %>% dplyr::select(id,latitude,longitude)
restaurant1 = read.csv("restaurant.csv")
restaurant1 = restaurant1 %>% filter(as.numeric(str_sub(restaurant1$INSPECTION.DATE,-4,-1))>=2019) %>% 
  drop_na() %>%
  head(5000)
restaurant = restaurant1 %>% 
  dplyr::select(DBA, Latitude, Longitude, CUISINE.DESCRIPTION)
crime  = read.csv("crime.csv") %>% 
  mutate(OCCUR_DATE = as.character(OCCUR_DATE) %>% as.Date("%m/%d/%Y")) %>%
  filter(year(OCCUR_DATE)>=2019)


#### data preprocessing
### tab1_search
number_houses = tab1_data %>%
  group_by(id) %>%
  count()

number_hosts = length(unique(tab1_data$host_id))

number_reviews = tab1_data %>%
  group_by(id) %>%
  distinct(number_of_reviews) %>%
  summarise(reviews = sum(number_of_reviews))
number_reviews = sum(number_reviews$reviews)

### tab3_summary
## top expensive/cheap
tab3_data = tab1_data %>%
  select(id, neighbourhood_group, price.x) %>%
  group_by(id) %>%
  mutate(price = round(mean(price.x))) %>%
  select(id, neighbourhood_group, price)
tab3_data = distinct(tab3_data)

tab3_data_expensive = tab3_data %>%
  filter(price > 0) %>%
  group_by(neighbourhood_group) %>%
  arrange(desc(price)) %>%
  top_n(10, price)
tab3_data_expensive$id <- factor(tab3_data_expensive$id, levels = tab3_data_expensive$id[order(tab3_data_expensive$price)])


tab3_data_cheap = tab3_data %>%
  filter(price > 0) %>%
  group_by(neighbourhood_group) %>%
  arrange(desc(price)) %>%
  top_n(-10, price)
tab3_data_cheap$id <- factor(tab3_data_cheap$id, levels = tab3_data_cheap$id[order(-tab3_data_cheap$price)])


## price
data1=read.csv("analysisData.csv",header=TRUE, stringsAsFactors = FALSE)
data1$last_review = as.Date(data1$last_review)
data1$month<-month(data1$last_review)

data1_new=data1%>%
  dplyr::select(price,neighbourhood_group_cleansed,month)%>%
  rename(neighbourhood_group=neighbourhood_group_cleansed)

price_data<-  data1_new%>%
  group_by(neighbourhood_group,month) %>% 
  mutate(mean_price=as.numeric(mean(price)))%>%
  ungroup()%>%
  arrange(month)
priceTS <- ggplot(price_data, aes(month,mean_price, color = neighbourhood_group))+
  geom_line(aes(group = neighbourhood_group))+
  ggtitle("Price variability by time/neighborhood")+
  xlab("month")+
  scale_x_continuous(limits=c(1,12), breaks=seq(1,12,1))+
  guides(fill = guide_legend(title = "Neighbourhood group"))+
  theme_set(theme_bw())+
  theme(panel.grid.major=element_line(colour=NA))+
  theme_classic() + 
  theme(axis.line = element_blank()) + 
  theme(legend.position="bottom")

data2_new=data1%>%
  dplyr::select(price,room_type,month)
price_data2<-  data2_new%>%
  group_by(room_type,month) %>% 
  mutate(mean_price=as.numeric(mean(price)))%>%
  ungroup()%>%
  arrange(month)
priceTS2=ggplot(price_data2, aes(month,mean_price, color = room_type))+
  geom_line(aes(group = room_type))+
  ggtitle("Price variability by time/room_type")+
  xlab("month")+
  scale_x_continuous(limits=c(1,12), breaks=seq(1,12,1))+
  guides(fill = guide_legend(title = "room_type"))+
  theme_set(theme_bw())+
  theme(panel.grid.major=element_line(colour=NA)) + 
  theme_classic() + 
  theme(axis.line = element_blank()) + 
  theme(legend.position="bottom")

### build dashboard

header <- dashboardHeader(
  title = tags$img(src="airbnb logo.jpg"),
  dropdownMenu(
    type = "notifications",
    notificationItem(
      text = "10 people are looking at the same house",
      icon("users")
    ),
    notificationItem(
      text = "The rental price is decreasing!",
      icon("dollar-sign")
    )
  )                                                     
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Search", tabName = "search", icon = icon("search-location")),
    menuItem("Environment", tabName = "crime_resturant", icon = icon("utensils")),
    menuItem("Overview", tabName = "descriptive", icon = icon("chart-line")),
    menuItem("References", tabName = "references", icon = icon("th"))
  )
)

body <- dashboardBody(
  ## set the color of header
  tags$head(tags$style(HTML('/* logo */
                                .skin-blue .main-header .logo {
                                background-color: #fd5c63;
                                }
                            /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #fd5c63;
                                }
                            /* navbar (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #fd5c63;
                            }
                            /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #fd5c63;
                            }
                            /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #f8f9f9;
                            }
                            /* body */
                                .content-wrapper, .right-side {
                                background-color: #f8f9f9;
                            }
                            /*    Move everything below the header */
                            .content-wrapper {
                            margin-top: 100px;
                            }
                            .content {
                            padding-top: 50px;
                            }
                            /*    Format the title/subtitle text */
                            .title-box {
                            position: absolute;
                            text-align: center;
                            top: 50%;
                            left: 50%;
                            transform:translate(-50%, -50%);
                            }
                            @media (max-width: 590px) {
                            .title-box {
                            position: absolute;
                            text-align: center;
                            top: 10%;
                            left: 10%;
                            transform:translate(-5%, -5%);
                            }
                            }
                            @media (max-width: 767px) {
                            .primary-title {
                            font-size: 1.1em;
                            }
                            .primary-subtitle {
                            font-size: 1em;
                            }
                            }
                            /*    Make the image taller */
                            .main-header .logo {
                             height: 190px;
                              }
                            /*    Override the default media-specific settings */
                            @media (max-width: 5000px) {
                            .main-header {
                            padding: 0 0;
                            position: relative;
                            }
                            .main-header .logo,
                            .main-header .navbar {
                            width: 100%;
                            float: none;
                            }
                           .main-header .navbar {
                           margin: 0;
                           }
                           .main-header .navbar-custom-menu {
                          float: right;
                          }
                          }
                          /*    Move the sidebar down */
                          .main-sidebar {
                          position: absolute;
                          }
                          .left-side, .main-sidebar {
                          padding-top: 250px;
                          }'
  ))),
  tabItems(
    tabItem(
      tabName = "home",
      fluidPage(
        tags$style(
          HTML('
          .box.box-solid.box-primary>.box-header {
            color:#fff;
              background:#fd5c63
          }
          
          .box.box-solid.box-primary{
            border-bottom-color:#fd5c63;
              border-left-color:#fd5c63;
              border-right-color:#fd5c63;
              border-top-color:#fd5c63;
          }'
          )
        ),
        fluidRow(
          box(width = 15, title = "Introduction", status = "primary",
              solidHeader = TRUE, 
              h3("Airbnb Exploration - Help you find the ideal house"),
              h4("Airbnb has become one of the most popular platform for people to list, discover, and book accommodations around the world. People can find various types of houses through airbnb app. However, users can only see the internal pictures, the price per night, and the overall introduction of the house. They cannot clearly know the environment of the house like crime and restaurant situation. Also, users cannot search for multiple places at one time."),
              h4("Our shiny app is based on airbnb houses in NYC. It aims to help airbnb users who are totally new to NYC, and want to know more about their ideal houses and the rental situation in NYC. They can check the crime situation, restaurants around, the average price of different boroughs, the most expensive house in each area, etc."), 
              h4("Let's explore this app!"))),
        fluidRow(
          box(width = 15, title = "User Guide", status = "primary",
              solidHeader = TRUE,
              h3("How to use this app?"),
              tags$div(tags$ul(
                tags$li("Search: This part is our search map and it contains four filters: borough, price range, room type, and date range. For each house users are interested in, they can know the house id, price per night, and minimum nights by click on the house marker. Also, users can copy the ID to the second part - environment to explore the crime and restaurant of the house. On the top of the map, people can also see the overall airbnb booking situation in NYC, which could motivate users to order."),
                tags$li("Environment: Users can search nearby restaurant and crime situation based on the distance range. They can even choose their favoriate cuisine type or explore the crime details like victim age group. This can provide them with a clear understanding of around situation and help them make the decision."),
                tags$li("Overview: The tab has several graphs to give users a board overview of rental situation in NYC. Some tourists might feel shocked when they see the rental price of NYC. Therefore, we provide the price fluctuation across different boroughs and room type. Moreover, the total number of houses distribution by areas are given. Based on customers' preference, we also offer the top 10 expensive and cheap houses' ids for them to check.")
              ))
          )
        ),
        fluidRow(
          tags$img(
            src = "new york2.jpg",
            width = "100%"
          )
        )
      )
    ),
    tabItem(
      tabName = "search",
      fluidPage(
        fluidRow(
          valueBox(
            value = dim(number_houses)[1],
            subtitle = "houses are left",
            icon = icon("home"),
            color = "light-blue",
            width = 4
          ),
          valueBox(
            value = round(mean(tab1_data$price.x),0),
            subtitle = "per night on average",
            icon = icon("dollar-sign"),
            color = "aqua",
            width = 4
          ),
          valueBox(
            value = number_reviews,
            subtitle = "reviews for your reference",
            icon = icon("comment-dots"),
            color = "teal",
            width = 4
          )),
        fluidRow(
          column(2,
                 checkboxGroupInput("neighborhood", "House area",
                                    c("Bronx" = "Bronx",
                                      "Brooklyn" = "Brooklyn",
                                      "Manhattan" = "Manhattan",
                                      "Queens" = "Queens",
                                      "Staten Island" = "Staten Island")),
                 sliderInput("price", label = h3("Price range"), min = 0, max = 10000, 
                             value = c(100,500)),
                 checkboxGroupInput("roomtype", label = h3("Room type"),
                                    choices=list("Entire home/apartment" = "Entire home/apt", "Private room" = "Private room",
                                                 "Shared room" = "Shared room"), selected = "Private room"),
                 dateRangeInput('dateRange',
                                label = 'Date range',
                                start = "2020-02-28", end = "2020-06-30"),
                 submitButton("Submit",width='100%'),div()),
          column(10, leafletOutput("plot",height="800px"),div())
        )
      )
    ),
    tabItem(
      tabName = "crime_resturant",
      fluidPage(
        navbarPage(
          "The Surroundings",
          tabPanel(
            "Restaurant",
            fluidRow(
              titlePanel(h3("Do you wanna know the restaurants near your house?")),
              br(),
              br(),
              column(2,wellPanel(
                br(),
                numericInput("id", h4("House_id:"),value = 2539),
                sliderInput(inputId="dist",h4("Distance Range:"), min = 0, max=5, value=2.5,step=0.5),
                selectInput("cuisine_type",h4("Cuisine Type"),choices=c("Japanese","American","Spanish","Italian","Korean","Chinese","French","Thai","Mexican","Indian"),multiple = F, selected = "American"),
                submitButton("Submit",width='100%'),
                style="background-color: #ffffff"
              )),
              column(10,leafletOutput("rplot",height="800px"))
            )
          ),
          tabPanel(
            "Crime",
            fluidRow(
              titlePanel(
                h3("Is your house safe?")),
              br(),
              h4("Let's get familiar about the crime events near your house."),
              br(),
              br(),
              column(2,wellPanel(
                style = "overflow-y:scroll; background-color: #ffffff;",
                numericInput("c.id", h4("House_id:"),value = 2539),
                sliderInput(inputId="c.dist",h4("Distance Range:"), min = 0, max=5, value=2.5,step=0.5),
                checkboxGroupInput("added_makers","Victim Age Group:",
                                   choices=c("<18","18-24","25-44","45-64","65+","UNKNOWN"),
                                   selected = "<18"),
                actionButton("click_all_age_group","Select All"),
                actionButton("click_no_age_group","Clear Options"),
                br(),
                br(),
                submitButton("Submit",width='100%')
              )),
              column(10,leafletOutput("cplot",height="800px"))
            ),
            style="opacity = 0.8"
          )
        )
      )
    ),
    tabItem(
      tabName = "descriptive",
      tabsetPanel(type = "tabs",
                  tabPanel(
                    "Airbnb's house",
                    fluidRow(column(width =  12,h3("Price variability by time/neighborhood"), 
                                    plotlyOutput("price_neighborhood"))),
                    fluidRow(),
                    fluidRow(column(width =  12, h3("Price variability by time/room_type"), 
                                    plotlyOutput("price_roomtype"))),
                    fluidRow(),
                    fluidRow(),
                    fluidRow(column(12, 
                                    h3("Roomtype distribution across boroughs"), 
                                    "By default, the bar chart shows the distribution of a specific room type by number as the height of each bar, and pie chart shows the percentage of a specific room type in the total available houses."
                    )),
                    fluidRow(column(width = 12, h3("The top 10 expensive or cheap houses in each area"))),
                    fluidRow(
                      column(2,
                             radioButtons("tab3_neighborhood", "Area", 
                                          c("Manhattan" = "Manhattan",
                                            "Bronx" = "Bronx",
                                            "Brooklyn" = "Brooklyn",
                                            "Queens" = "Queens",
                                            "Staten Island" = "Staten Island")),
                             submitButton("Submit",width='100%'),div()
                      ),
                      column(5, plotlyOutput('area_plot_1'), div()),
                      column(5, plotlyOutput('area_plot_2'), div())
                    )
                  ),
                  tabPanel(
                    "NYC's Crime",
                    fluidRow(
                      h3("Crime Distribution in each Boro"),
                      plotlyOutput("crime_pie"),
                      br(),
                      h3("Crime Distribution from January to September"),
                      plotlyOutput("crime_hist",height = 300,width = "100%")
                    )
                  ),
                  tabPanel("NYC's Restaurant",
                           fluidRow(h3("Choose the Cuisine Type you wanna know:")),
                           fluidRow(selectInput("cuisine_type.hist", "Cuisine type",
                                                choices=c("Japanese","American","Spanish","Italian","Korean","Chinese","French","Thai","Mexican","Indian"),
                                                multiple = F,
                                                selected = "American",
                                                width = "20%")),
                           fluidRow(submitButton("Submit",width='20%')),
                           br(),
                           fluidRow(plotlyOutput("restaurant_cuisine")),
                           fluidRow(h3("Choose the Restaurant with High Inspection Mark")),
                           fluidRow(selectInput("grade_type","Grade type",choices=c("A","B","C"),multiple = F,selected = "A",width = "20%")),
                           fluidRow(submitButton("Submit",width='20%')),
                           br(),
                           fluidRow(plotlyOutput("grade")
                           )
                           
                  )
      )),
    tabItem(
      tabName = "references",
      fluidPage(
        fluidRow(
          box(width = 15, title = "Data Source", status = "primary",
              solidHeader = TRUE,
              "The data source of this shiny app is from",
              tags$a(href = "https://opendata.cityofnewyork.us/", "NYC Open Data"), 
              ".")
        ),
        fluidRow(
          box(width = 15, title = "Project Code", status = "primary",
              solidHeader = TRUE, 
              "The code of this project can be found at",
              actionButton(inputId='code', label="GitHub", 
                           icon = icon("github"), 
                           onclick ="window.open('https://github.com/TZstatsADS/Spring2020-Project2-group-10')"),
              ".")
        ),
        fluidRow(
          box(width = 15, title = "Contact Us", status = "primary",
              solidHeader = TRUE, 
              h4("Feel free to contact us if you're interested in this app!"),
              h5("Wang, Yuyao: yuyaow@bu.edu"), 
              h5("Zhang, Qin: qz2387@columbia.edu"), 
              h5("Zhang, Xinlin xz2863@columbia.edu")
          )
        )
        ,
        fluidRow(
          tags$img(
            src = "airbnb_guide.jpg",
            width = "100%"
          )
        )
      )
    )
  )
)



# Define UI for application that draws a histogram
ui <- dashboardPage(header, sidebar, body)

server <- function(input, output,session) {
  filtered_tab1 = reactive({
    tab1_data %>%
      filter(neighbourhood_group %in% input$neighborhood) %>%
      filter(price.x >= input$price[1] & price.x <= input$price[2]) %>%
      filter(room_type %in% input$roomtype) %>%
      filter(start_date >= input$dateRange[1] & end_date <= input$dateRange[2])
  })
  
  output$plot <- renderLeaflet({
    tab1 = filtered_tab1()
    leaflet(options = leafletOptions(minZoom = 8, maxZoom = 18)) %>%
      addTiles() %>%
      setView(lng = -74.00, lat = 40.71, zoom = 12) %>%
      addMarkers(
        lng = tab1$longitude,
        lat = tab1$latitude,
        popup = paste("ID:",tab1$id,"<br/>",
                      "Price:",tab1$price.x, "<br/>",
                      "Minimum nights:",tab1$minimum_nights.x, "<br/>"),
        clusterOptions = markerClusterOptions()) %>%
      addProviderTiles("CartoDB.Positron")
  })
  id_r <- reactive({input$id})
  cuisine_type_r <- reactive({input$cuisine_type})
  dist_r <- reactive({input$dist})
  output$rplot <- renderLeaflet({
    current_location <- airbnb %>% filter(id==id_r()) %>% dplyr::select(longitude,latitude)
    filtered_restaurant <- restaurant %>% filter(CUISINE.DESCRIPTION %in% cuisine_type_r())
    locations = as.matrix(cbind(filtered_restaurant$Longitude,filtered_restaurant$Latitude))
    distances = distHaversine(locations, current_location)
    data = filtered_restaurant[distances<dist_r()*1609.34,]
    leaflet(data) %>%
      addTiles() %>%
      setView(lng = current_location$longitude, 
              lat= current_location$latitude, 
              zoom = 12) %>%
      addMarkers(lng = ~current_location$longitude,
                 lat = ~current_location$latitude, 
                 popup = "Chosen House",
                 icon = list(iconUrl = 'https://cdn2.iconfinder.com/data/icons/unigrid-phantom-buildings-vol-1/60/008_041_home_apartment_house_building_3-512.png'
                             ,iconSize = c(25,25))
      ) %>%
      addCircles(lng = ~current_location$longitude,
                 lat = ~current_location$latitude, 
                 radius = dist_r()*1609.34) %>%
      addMarkers(lng = ~Longitude, 
                 lat = ~Latitude, 
                 popup = paste(
                   "Name:", data$DBA, "<br>",
                   "Type:", data$CUISINE.DESCRIPTION
                 ),
                 icon=list(iconUrl = 'https://cdn2.iconfinder.com/data/icons/food-solid-icons-volume-1/128/030-512.png'
                           ,iconSize = c(25,25)))%>%
      addProviderTiles("CartoDB.Positron")
    
    
  })
  # define reactive
  click_all_age_group_r <- reactive({input$click_all_age_group})
  click_no_age_group_r <- reactive({input$click_no_age_group})
  c.id_r <- reactive({input$c.id})
  added_makers_r <- reactive({input$added_makers})
  c.dist_r <- reactive({input$c.dist})
  
  observeEvent(click_all_age_group_r(), {
    updateCheckboxGroupInput(session, "added_makers",
                             choices = c("<18","18-24","25-44","45-64","65+","UNKNOWN"),
                             selected = c("<18","18-24","25-44","45-64","65+","UNKNOWN"))
  })
  observeEvent(click_no_age_group_r(), {
    updateCheckboxGroupInput(session, "added_makers",
                             choices = c("<18","18-24","25-44","45-64","65+","UNKNOWN"),
                             selected = NULL)
  })
  output$cplot <- renderLeaflet({
    c.current_location <- airbnb %>% filter(id==c.id_r()) %>% dplyr::select(longitude,latitude)
    filtered_crime <- crime %>% filter(VIC_AGE_GROUP %in% added_makers_r())
    c.locations = as.matrix(cbind(filtered_crime$Longitude,filtered_crime$Latitude))
    c.distances = distHaversine(c.locations, c.current_location)
    c.data = filtered_crime[c.distances<c.dist_r()*1609.34,]
    leaflet(c.data) %>%
      addTiles() %>%
      setView(lng = c.current_location$longitude, 
              lat= c.current_location$latitude, 
              zoom = 12) %>%
      addMarkers(lng = ~c.current_location$longitude,
                 lat = ~c.current_location$latitude, 
                 popup = "Chosen House",
                 icon = list(iconUrl = 'https://cdn2.iconfinder.com/data/icons/unigrid-phantom-buildings-vol-1/60/008_041_home_apartment_house_building_3-512.png'
                             ,iconSize = c(25,25))) %>%
      addCircles(lng = ~c.current_location$longitude,
                 lat = ~c.current_location$latitude, 
                 radius = c.dist_r()*1609.34) %>%
      addMarkers(lng = ~Longitude, 
                 lat = ~Latitude, 
                 popup = paste(
                   "Occur time: ", c.data$OCCUR_TIME, "<br>",
                   "Victim Gender:", c.data$VIC_SEX,"<br>",
                   "Victim Age:", c.data$VIC_AGE_GROUP
                 ),
                 icon=list(iconUrl = 'https://cdn0.iconfinder.com/data/icons/crime-investigation-basic-lineal-color/512/41_Revolver-512.png'
                           ,iconSize = c(25,25)),
                 clusterOptions = markerClusterOptions())%>%
      addProviderTiles("CartoDB.Positron")
    
    
  })
  output$cplot <- renderLeaflet({
    c.current_location <- airbnb %>% filter(id==input$c.id) %>% dplyr::select(longitude,latitude)
    filtered_crime <- crime %>% filter(VIC_AGE_GROUP %in% input$added_makers)
    c.locations = as.matrix(cbind(filtered_crime$Longitude,filtered_crime$Latitude))
    c.distances = distHaversine(c.locations, c.current_location)
    c.data = filtered_crime[c.distances<input$c.dist*1609.34,]
    leaflet(c.data) %>%
      addTiles() %>%
      setView(lng = c.current_location$longitude, 
              lat= c.current_location$latitude, 
              zoom = 12) %>%
      addMarkers(lng = ~c.current_location$longitude,
                 lat = ~c.current_location$latitude, 
                 popup = "Chosen House",
                 icon = list(iconUrl = 'https://cdn2.iconfinder.com/data/icons/unigrid-phantom-buildings-vol-1/60/008_041_home_apartment_house_building_3-512.png'
                             ,iconSize = c(25,25))) %>%
      addCircles(lng = ~c.current_location$longitude,
                 lat = ~c.current_location$latitude, 
                 radius = input$c.dist*1609.34) %>%
      addMarkers(lng = ~Longitude, 
                 lat = ~Latitude, 
                 popup = paste(
                   "Occur time: ", c.data$OCCUR_TIME, "<br>",
                   "Victim Gender:", c.data$VIC_SEX,"<br>",
                   "Victim Age:", c.data$VIC_AGE_GROUP
                 ),
                 icon=list(iconUrl = 'https://cdn0.iconfinder.com/data/icons/crime-investigation-basic-lineal-color/512/41_Revolver-512.png'
                           ,iconSize = c(30,30)),
                 clusterOptions = markerClusterOptions())%>%
      addProviderTiles("CartoDB.Positron")
    
    
  })
  # crime
  colors <- c("#74d2e7", "#2dde98", "#ffc168", "#ff6c5f", "#8e43e7")
  output$crime_pie <- renderPlotly({
    crime_pie.data <- crime %>%
      dplyr::group_by(BORO) %>%
      dplyr::summarize(n=n()) %>%
      arrange(BORO)
    plot_ly() %>% add_pie(data = crime_pie.data, 
                          labels = ~BORO, 
                          values = ~n,
                          name = names(crime_pie.data)[1],
                          domain = list(row = 0, column = 0),
                          marker = list(colors = colors))%>%
      layout(plot_bgcolor='transparent') %>%
      layout(paper_bgcolor='transparent')
  })
  output$crime_hist <- renderPlotly({
    crime_hist.data <-
      crime %>%
      dplyr::mutate(month=month(OCCUR_DATE)) %>%
      dplyr::select(INCIDENT_KEY,BORO,month) %>%
      dplyr::group_by(month,BORO) %>%
      dplyr::arrange(BORO)
    plot_ly(crime_hist.data, x=~month, color=~BORO) %>%
      add_histogram() %>%
      layout(title="Histogram for crime in each month",
             xaxis = list(title="Month",zeroline=F),
             yaxis = list(title="Count",zeroline=F))%>%
      layout(plot_bgcolor='transparent') %>%
      layout(paper_bgcolor='transparent')
  })
  #grade_pie
  # define reactive
  grade_type_r <- reactive({input$grade_type})
  cuisine_type.hist_r <- reactive({input$cuisine_type.hist})
  output$grade <- renderPlotly({
    restaurant_boro <- restaurant1 %>% 
      dplyr::select(DBA, BORO, CUISINE.DESCRIPTION, GRADE) %>% 
      dplyr::filter(GRADE != "", BORO != 0) %>%
      dplyr::filter(CUISINE.DESCRIPTION %in% c("Japanese","American","Spanish","Italian","Korean",
                                               "Chinese","French","Thai","Mexican","Indian")) %>%
      dplyr::filter(GRADE != "P",GRADE != "N", GRADE != "Z") %>%
      dplyr::group_by(BORO, GRADE) %>%
      dplyr::summarise(count=n())
    r_boro_a <- restaurant_boro %>%
      dplyr::filter(GRADE=="A")
    r_boro_b <- restaurant_boro %>%
      dplyr::filter(GRADE=="B")
    r_boro_c <- restaurant_boro %>%
      dplyr::filter(GRADE=="C")
    if(grade_type_r()=="A"){
      plot_ly() %>% add_pie(data = r_boro_a, 
                            labels = ~BORO, 
                            values = ~count,
                            name = names(restaurant_boro)[1],
                            domain = list(row = 0, column = 0),
                            marker = list(colors = colors))%>%
        layout(plot_bgcolor='transparent') %>%
        layout(paper_bgcolor='transparent')
    }
    else if(grade_type_r()=="B"){
      plot_ly() %>% add_pie(data = r_boro_b, 
                            labels = ~BORO, 
                            values = ~count,
                            name = names(restaurant_boro)[1],
                            domain = list(row = 0, column = 0),
                            marker = list(colors = colors))%>%
        layout(plot_bgcolor='transparent') %>%
        layout(paper_bgcolor='transparent')
      
    }
    else if(grade_type_r()=="C"){
      plot_ly() %>% add_pie(data = r_boro_c, 
                            labels = ~BORO, 
                            values = ~count,
                            name = names(restaurant_boro)[1],
                            domain = list(row = 0, column = 0),
                            marker = list(colors = colors))%>%
        layout(plot_bgcolor='transparent') %>%
        layout(paper_bgcolor='transparent')
    }
  })
  filtered_tab3_area_1 = reactive({
    tab3_data_expensive %>%
      filter(neighbourhood_group == input$tab3_neighborhood) %>%
      select(id, price) 
  })
  output$area_plot_1  = renderPlotly({
    tab3_area_1 = filtered_tab3_area_1()
    a = ggplot(tab3_area_1, aes(x = id, y=price))+
      geom_bar(stat = "identity", fill = "#fd5c63") + 
      ggtitle("Top 10 expensive hourse's id") +
      coord_flip() +
      theme_classic() + 
      theme(axis.line = element_blank()) + 
      xlab("ID of house") +
      ylab("Price")
    fig_a = ggplotly(a) %>%
      layout(showlegend = FALSE, plot_bgcolor='transparent') %>%
      layout(paper_bgcolor='transparent')
  })
  filtered_tab3_area_2 = reactive({
    tab3_data_cheap %>%
      filter(neighbourhood_group == input$tab3_neighborhood) %>%
      select(id, price)
  })
  output$area_plot_2  = renderPlotly({
    tab3_area_2 = filtered_tab3_area_2()
    b = ggplot(tab3_area_2, aes(x = id, y=price))+
      geom_bar(stat = "identity", fill = "#fd5c63") + 
      ggtitle("Top 10 cheap hourse's id") +
      coord_flip() +
      theme_classic() + 
      theme(axis.line = element_blank()) + 
      xlab("ID of house") +
      ylab("Price")
    fig_b = ggplotly(b) %>% 
      layout(showlegend = FALSE, plot_bgcolor='transparent') %>%
      layout(paper_bgcolor='transparent')
  })
  output$price_neighborhood<- renderPlotly({
    ggplotly(priceTS) %>%
      layout(legend = list(bgcolor = "transparent",
                           bordercolor = "transparent")) %>%
      layout(plot_bgcolor='transparent') %>%
      layout(paper_bgcolor='transparent')
  })
  output$price_roomtype<- renderPlotly({
    ggplotly(priceTS2) %>%
      layout(legend = list(bgcolor = "transparent",
                           bordercolor = "transparent")) %>%
      layout(plot_bgcolor='transparent') %>%
      layout(paper_bgcolor='transparent')
  })
  output$restaurant_cuisine <- renderPlotly({
    restaurant_cuisine.data <-  restaurant1 %>% 
      dplyr::select(DBA, BORO, CUISINE.DESCRIPTION, GRADE) %>% 
      dplyr::filter(GRADE != "", BORO != 0) %>% 
      dplyr::select(BORO,CUISINE.DESCRIPTION) %>%
      dplyr::filter(CUISINE.DESCRIPTION %in% c("Japanese","American","Spanish","Italian","Korean",
                                               "Chinese","French","Thai","Mexican","Indian")) %>%
      dplyr::filter(CUISINE.DESCRIPTION %in% cuisine_type.hist_r()) %>%
      dplyr::group_by(CUISINE.DESCRIPTION,BORO) %>%
      dplyr::summarise(n=n())
    plot_ly(restaurant_cuisine.data, x=~BORO, y=~n,marker = list(color = "fd5c63")) %>%
      add_bars() %>%
      layout(title="The number of restaurant in each Boro",
             xaxis = list(title="BORO",zeroline=F),
             yaxis = list(title="Count",zeroline=F)) %>%
      layout(showlegend = FALSE, plot_bgcolor='transparent') %>%
      layout(paper_bgcolor='transparent')
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
