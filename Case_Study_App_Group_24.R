#Shiny App Group 24 

#Load packages
if (!require(install.load) ){
  installed.packages("install.load")
  library(install.load)
}

install_load("tidyverse", "shiny", "leaflet", "shinydashboard", "dplyr", "fresh", "ggplot2")

#Create theme for the dashboard
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#b0c4de" #Color: lightsteelblue
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#b0c4de"
  )
)



#Load data frames
abj <- read_csv("Anzahl_betroffen_Jahr.csv")
fds <- read_csv("Final_dataset_group_24.csv")

#Set parameter
abj$Zulassungsjahr <- as.numeric(abj$Zulassungsjahr)

#Parameter for palette
range_bin <- seq(1, 701, by=50)
mypalette <- colorBin( palette="YlOrBr", domain=abj$Anzahl_betroffen_Jahr, na.color="transparent", bins=range_bin)


#Create UI
ui <- dashboardPage(
  
  #Create dashboard header
  dashboardHeader(title = "Case Study Group 24 - Company 106"),
  
  #Create sidebar with items "Plot", "Heatmap", "Table"
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plot", tabName = "plot", icon = icon("chart-line")),
      menuItem("Heatmap", tabName = "heatmap", icon = icon("map")),
      menuItem("Table", tabName = "table" , icon = icon("table"))
    )
  ),
  
  #Create dashboard body
  dashboardBody(
    
    #Use predefined theme
    use_theme(mytheme),
    
    #Use logo from the source (https://isis.tu-berlin.de/pluginfile.php/2324148/course/section/466300/QW_print.png)
    tags$img(align="left",src="QW_print.png",height="50px"),
    br(),
    br(),
    br(),
    tabItems(
      
      #Create first tab content: "Plot"
      tabItem(tabName = "plot",
              
              fluidRow(
                sidebarLayout(
                  
                  #Create sidebar panel
                  sidebarPanel(
                    
                    #Create input options: "select input" and "slider input"
                    selectInput("locations_p", "Gemeinde:", multiple = TRUE, choices = unique(abj$Gemeinde), selected = c("KOELN", "DORTMUND", "LEIPZIG", "BREY"), selectize = TRUE),
                    sliderInput("years_p", "Jahr:", min = 2009, max = 2016, value = c(2009, 2016), sep = ""),
                  ),
                  
                  #Create main panel: "Plot" output
                  mainPanel(
                    plotOutput(outputId = "plot", width = '100%', height ='500px')
                  ),position = "left"
                  
                )
              )
      ),
      
      #Create second tab content: "Heatmap"
      tabItem(tabName = "heatmap",
              
              fluidRow(
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("year_h1", "Jahr:", value = 2009, min = 2009, max = 2016, sep = "",
                                animate = animationOptions(interval = 1500))
                  ),
                  
                  mainPanel(
                    leafletOutput("map", width = '100%', height ='800px' )
                  )
                )
              )
      ),
      
      #Create third tab content: "table"
      tabItem(tabName = "table",
              
              fluidPage(
                titlePanel("Finaler Datensatz"),
                
                fluidRow(
                  column(4,
                         selectInput("vehicle",
                                     "Fahrzeugtyp:",
                                     c("Beide",unique(as.character(fds$Fahrzeug_Typ)))
                         )
                  ),
                  column(4,
                         selectInput("location",
                                     "Gemeinde:",
                                     c("Alle",unique(as.character(fds$Gemeinde)))))
                  
                ),
                DT::dataTableOutput("table")
              )
      )
      
    )
  )
)


#Define Server
server <- function(input, output, session) {
  
  #Load data
  abj <- read_csv("Anzahl_betroffen_Jahr.csv")
  
  #Create reactive function for Plot to filter years and locations
  filtereddata <- reactive({
    abj %>%
      filter(Gemeinde %in% input$locations_p,
             between(Zulassungsjahr, input$years_p[1], input$years_p[2]))
  })
  
  #Plot function
  output$plot <- renderPlot({
    filtereddata() %>%      
      ggplot(aes(x =Zulassungsjahr , y = Anzahl_betroffen_Jahr, color = Gemeinde ))+
      geom_line()+
      geom_point(size = 4)+
      labs(x = "Jahr", y = "Anzahl betroffener Fahrzeuge") +
      scale_x_continuous(labels = function(x) format(x, big.mark = "", decimal.mark = "")) 
  })
  
  #Create reactive function for Heatmap to filter year
  filtered_data_h <- reactive({
    abj %>%
      filter(input$year_h1 == Zulassungsjahr)
  })
  
  #Create Heatmap
  output$map <- renderLeaflet({
    leaflet(filtered_data_h(),
            options = leafletOptions(dragging = TRUE,
                                     minZoom = 2,
                                     maxZoom = 12)
            ) %>%
      addTiles() %>%
      
      #Set default view to Germany
      setView(lat=51.5, lng=10 , zoom=6) %>%
      
      #Add different colored markers according to palette and sizes according to "Anzahl_betroffen_Jahr"
      addCircleMarkers(lng = filtered_data_h()$Laengengrad, lat = filtered_data_h()$Breitengrad,
                       popup = paste("Gemeinde:", filtered_data_h()$Gemeinde, 
                                     br(), "Postleitzahl:", filtered_data_h()$Postleitzahl, 
                                     br(), "Anzahl fehlerhafter Fahrzeuge:", filtered_data_h()$Anzahl_betroffen_Jahr),
                       col = ~mypalette(filtered_data_h()$Anzahl_betroffen_Jahr), fillOpacity = 0.7, radius = sqrt(2*filtered_data_h()$Anzahl_betroffen_Jahr), stroke = FALSE) %>%
      
      #Add legend and position it in topright
      addLegend( pal=mypalette, values=~filtered_data_h()$Anzahl_betroffen_Jahr, opacity=0.85, title = "Anzahl betroffener Fahrzeuge", position = "topright" )
  })
  
  #Table function
  output$table <- DT::renderDataTable(DT::datatable({
    data <- fds[-c(1)]
    if (input$vehicle != "Beide" ){
      data <- data[data$Fahrzeug_Typ == input$vehicle,]
    }
    if (input$location != "Alle" ){
      data <- data[data$Gemeinde == input$location,]
    }
    data
  }
  , options = list(scrollX = TRUE) #Put scroll option
  ))
  
}

shinyApp(ui, server)

