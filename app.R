#Libraries ----
library(tidyverse)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(patchwork)
library(plotly)

library(Hmisc)
library(car)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(plyr)
library(psych)

library(mapproj)
library(ggplot2)
library(ggmap)
library(maptools)
library(ggthemes)
library(rgeos)
library(broom)
library(dplyr)
library(plyr)
library(grid)
library(gridExtra)
library(reshape2)
library(scales)


#setwd("C:\\Users\\cakir\\Downloads")
sfAllListingsData <- read.csv("SFAllData.csv",na.strings = c("","?","NA"),stringsAsFactors=FALSE)
sfAllListingsData <- sfAllListingsData %>% 
    mutate(date = as.Date(date))

sfAllListingsData <- sfAllListingsData %>% 
  mutate(neighbourhood = case_when(neighbourhood == "Mission" ~ "Mission Dolores",
                                   neighbourhood == "Downtown/Civic Center" ~ "Downtown",
                                   neighbourhood == "Castro/Upper Market" ~ "South of Market",
                                   neighbourhood == "Ocean View" ~ "Oceanview",
                                   neighbourhood == "Financial District" ~ "Financial District/Barbary Coast",
                                   neighbourhood == "Seacliff" ~ "Sea Cliff",
                                   neighbourhood == "West of Twin Peaks" ~ "Twin Peaks",
                                   neighbourhood == "Golden Gate Park" ~ "Golden Gate Heights",
                                   neighbourhood == "Chinatown" ~ "Telegraph Hill",
                                   neighbourhood == "Lakeshore" ~ "Lake Shore",
                                   neighbourhood == "Presidio" ~ "Presidio Heights",
                                   neighbourhood == "Treasure Island/YBI" ~ "Yerba Buena",TRUE ~ neighbourhood))

num_cols <- unlist(lapply(sfAllListingsData,is.numeric))
names(num_cols)[num_cols] 

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    strip.text = element_text(size=12),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"))
}

# Define some palettes
palette_9_colors <- c("#0DA3A0","#2999A9","#458FB2","#6285BB","#7E7CC4","#9A72CD","#B768D6","#D35EDF","#F055E9")
palette_8_colors <- c("#0DA3A0","#2D97AA","#4D8CB4","#6E81BF","#8E76C9","#AF6BD4","#CF60DE","#F055E9")
palette_7_colors <- c("#2D97AA","#4D8CB4","#6E81BF","#8E76C9","#AF6BD4","#CF60DE","#F055E9")
palette_1_colors <- c("#0DA3A0")

URL <- "https://github.com/simonkassel/Visualizing_SF_home_prices_R/raw/master/Data/SF_neighborhoods.zip"
# Download the shapefile to your working directory and unzip it.
download.file(URL, "SF_neighborhoods.zip")
unzip("SF_neighborhoods.zip")
# Read it into R as a spatial polygons data frame & plot
neighb <- readShapePoly("SF_neighborhoods")

#Get basemapR
# Define the bounding box
bbox <- neighb@bbox

# Manipulate these values slightly so that we get some padding on our basemap between the edge of the data and the edge of the map
sf_bbox <- c(left = bbox[1, 1] - .01, bottom = bbox[2, 1] - .005, 
             right = bbox[1, 2] + .01, top = bbox[2, 2] + .005)
# Download the basemap
basemap <- get_stamenmap(
  bbox = sf_bbox,
  zoom = 13,
  maptype = "toner-lite")


neighb.tidy <- tidy(neighb, region = c('nbrhood'))
neighb.tidy$Neighborhood <- neighb.tidy$id

# UI ----
ui <- dashboardPage(
    skin = "purple",
    
    dashboardHeader(title = "San Francisco Airbnb Analysis",
                    titleWidth = 350),
    dashboardSidebar(
        width = 350,
        br(),
        h4("Select Your Inputs Here", style = "padding-left:18px"),
        hr(),
        uiOutput("sidebar")
        
    ), 
    dashboardBody(
        tabsetPanel(
            type = "tabs",
            id = "tab_selected",
            tabPanel(title = "Neighbourhood View",
                     plotlyOutput("plot_data_neighbourhood")),
            tabPanel(title = "Metrics View",
                     plotlyOutput("plot_metrics")),
            tabPanel(title = "Price Differential",
                     plotlyOutput("plot_data_pricedifferential"),
                     ),
            tabPanel(title = "Map View",
                     plotlyOutput("plot_neighbourhood_map"),
            )
        )
    )
)

# Server ----
server <- function(input, output) {
    # Plot section ----
    # 1. Metrics plot ----
    clean_data_price <- reactive({
        req(input$metric)
        sfAllListingsData %>%
            filter(neighbourhood %in% input$neighbourhood) %>%
            group_by(neighbourhood, date) %>%
            summarize_at(.vars = vars(input$metric),.funs = mean) %>% 
            select( neighbourhood, date, input$metric) %>%
            set_names( c("neighbourhood","date","metric")) %>% 
            arrange(date)
    })
    

    output$plot_data_neighbourhood <- renderPlotly({
        ggplotly(
            ggplot(data = clean_data_price(), aes(
                x = date,
                y = metric,
                color = neighbourhood
            )) +
                geom_line(size = 1) +
                geom_point() +
                theme_light() +
                theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
                scale_x_date(date_labels = "%Y-%m", date_breaks = "30 days")+
                theme(axis.text.x = element_text(angle = 90))+
                labs(x = "Neighbourhood",
                     y=input$metric,
                     title = input$metric)
        )
    })
    
    # 2. Metrics View plot ----
    get_metrics_data <- reactive({
        req(input$metric)
        sfAllListingsData %>%
            # filter(neighbourhood %in% input$neighbourhood) %>%
            group_by(room_type, date) %>%
            summarize_at(.vars = vars(input$metric),.funs = mean) %>% 
            mutate(date=as.Date(date)) %>% 
            select( room_type, date, input$metric) %>%
            set_names( c("room_type","date","metric")) %>% 
            arrange(date)
    })
    
    
    output$plot_metrics <- renderPlotly({
    p1 <-     ggplotly(
             ggplot(get_metrics_data(),aes(x=date, y=metric,group=room_type,color=room_type,size=metric)) + 
                geom_point() + 
                 scale_x_date(date_labels = "%Y-%m", date_breaks = "30 days")+
                 theme(axis.text.x = element_text(angle = 90))+
                 labs(x = "Date",
                      y=input$metric,
                      title = input$metric)
        )
            
            # p2 <- ggplotly( ggplot(sfAllListingsData) + 
            #     geom_boxplot(aes(neighbourhood, price, group = neighbourhood)) + 
            #     ggtitle('Plot 2'))
            # 
            # ,
            # 
            # p3 <- ggplot(mtcars) + 
            #     geom_point(aes(hp, wt, colour = mpg)) + 
            #     ggtitle('Plot 3'),
            # 
            # p4 <- ggplot(mtcars) + 
            #     geom_bar(aes(gear)) + 
            #     facet_wrap(~cyl) + 
            #     ggtitle('Plot 4'),
            # 
            #subplot(p1)
    p1
        
    })
    
    # 3. Price Differential plot ----
    
    get_price_diff <- reactive({
        req(input$diff)
        date_from_to <- unique(sfAllListingsData$date)
        top <- sfAllListingsData %>% 
            filter(date==date_from_to[1]) %>% 
            group_by(neighbourhood) %>% 
            summarize_at(.vars = vars(price),.funs = mean)
        bottom <- sfAllListingsData %>% 
            filter(date==date_from_to[as.numeric(input$diff)+1]) %>% 
            group_by(neighbourhood) %>% 
            summarize_at(.vars = vars(price),.funs = mean)
        joined <-  top %>% 
            left_join( bottom, by=c("neighbourhood")) %>% 
            mutate(price = price.x - price.y ) %>% 
            select(neighbourhood,price)
        
    })
    
    
    output$plot_data_pricedifferential <- renderPlotly({
        ggplotly(
            ggplot(data = get_price_diff(), aes(
                x = neighbourhood,
                y = price,fill=neighbourhood
            )) +
                geom_bar(stat = "identity") +
    
            theme_light() +
                theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
                
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        )
    })
    
    # 4. Neighbourhood Map View ----
    
    get_map_prices <- reactive({
    req(input$dates)
      
      sfAirbnb.summarized.date <- sfAllListingsData %>% 
        filter(date==input$dates) %>% 
        group_by(neighbourhood) %>%
        summarize_at(.vars = vars(price),.funs = mean) %>% 
        select( neighbourhood, price)
      
      sfAirbnb.summarized_date_map <- left_join(sfAirbnb.summarized.date, 
                                                neighb.tidy, 
                                                by = c("neighbourhood" = "Neighborhood"),
                                                match = "all")
      sfAirbnb.summarized_date_map %>% 
        select(everything())
    })
    
    
    output$plot_neighbourhood_map <- renderPlotly({
      ggplotly(
         ggmap(basemap) +
          geom_polygon(data = get_map_prices(),
                       aes(x = long, y = lat, group = group, fill = price),
                       colour = "white", alpha = 0.75, size = 0.25)
          +
          coord_map() +
          scale_fill_gradientn("Neighborhood \nAverage \nPrice",
                               colors = palette_8_colors,
                               labels = scales::dollar_format(prefix = "$")) +
          mapTheme() +
          theme(legend.position = "bottom",
                legend.direction = "horizontal",
                legend.key.width = unit(.5, "in")) +
          labs(title="Airbnb Prices, San Francisco",
               subtitle="Nominal prices (2020-06-08)",
               caption="Source: DSBA 6211 Group 4")
      )
    })
    
    # Inputs ----
    
    # Neighbourhood View ----
    # metric Input ----
    output$metric <- renderUI({ selectInput(
        inputId = "metric",
        label = "Select Metric to Compare",
        # choices = colnames(sfAllListingsData)[4:ncol(sfAllListingsData)],
        choices = names(num_cols)[num_cols],
        multiple = FALSE,
        selected = c("price")
    )})
    
    # neighbourhood Input ----
    output$neighbourhood <- renderUI({ selectInput(
        inputId = "neighbourhood",
        label = "Select neighbourhoods to Compare",
        choices = sort(unique(sfAllListingsData$neighbourhood)),
        multiple = TRUE,
        selected = c("Downtown", "Bayview")
        
    )})
    
    #Price Differential View ----
    
    output$diff <- renderUI({ radioButtons("diff", label="Choose time period(2020-06-08 to 2021-04-07):",
                 choices = list("1 Month" = 1,
                                "2 Months" = 2,
                                "3 Months" = 3,
                                "4 Months" = 4,
                                "5 Months" = 5,
                                "6 Months" = 6,
                                "7 Months" = 7,
                                "8 Months" = 8,
                                "9 Months" = 9,
                                "10 Months" = 10)
    )})
    
    #Map view ----
    output$dates <- renderUI({ radioButtons("dates", label="Choose time period(2020-06-08 to 2021-04-07):",
                                           choices = sort(unique(sfAllListingsData$date)),
    )})
    
    # actionButton("runDiff", "Run Differential")
    
    # sidebar ----
    
    output$sidebar <- renderUI({
        if(input$tab_selected == "Neighbourhood View"){
            div(
                uiOutput("metric"),
                uiOutput("neighbourhood")
            )
        }
        else if(input$tab_selected == "Metrics View"){
            div(
                uiOutput("metric")
            )
        }
        else if(input$tab_selected == "Price Differential"){
            div(
                uiOutput("diff")
            )
        }
      else if(input$tab_selected == "Map View"){
        div(
          uiOutput("dates")
        )
      }
    })
}

# Run the application ----
shinyApp(ui = ui, server = server)

