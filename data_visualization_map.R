##Loading the required R-packages:
#Again a help to process the data, this time it was supplemented with packages that are used for visualization.
pacman::p_load(
  rio,       # import/export
  here,      # filepaths
  lubridate, # working with dates
  plotly,    # interactive plots
  scales,    # quick percents
  tidyverse,  # data management and visualization
  shiny,
  leaflet,
  dplyr,
  sf
) 

#Load and preprocess data:
##The data was read in as csv files for further processing.
data <- read.csv("clean_data.csv")
data_long <- read_csv("clean_data.csv")

#Define UI:
#Allows you to choose a year between 1960 and 2020
#Allows the selection of a specific country or a worldwide view
#Defines the area for the interactive map
ui <- fluidPage(
  titlePanel("Fertility Rate of 186 Countries in 1960-2020"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "year",
        "Choose a year:",
        min = min(data$Year),
        max = max(data$Year),
        value = min(data$Year),
        step = 1,
        sep = "",
      ),
      selectInput("country", "Choose a country:", 
                  choices = c("Worldwide", unique(data_long$country_name)), 
                  selected = "Worldwide"
                  
      )
    ),
    mainPanel(
      leafletOutput("map", width = "100%", height = "600px")
    )
  )
)

#Define Server:
#The Shapefile contains the borders of the countries so that they can be displayed on the map
#Shapelift is loaded as an object so that it is compatible with the leaflet
server <- function(input, output, session) {
  world <- st_read("ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp", quiet = FALSE)
  
#Render initial map:
#Leaflet generates an interactive map
#addTiles adds the standard map tiles
#setView was set as start view with 10 degrees east, 30 degrees north and a zoom level of 2
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 10, lat = 30, zoom = 2)  # Standardansicht (kann angepasst werden)
  })
  
#Observe year input and render map:
#Observe updates the map when settings are changed
#Filter only shows data from the selected year
#Another filter is applied when a country is selected
  observe({
    filtered_data <- data %>% filter(Year == input$year)
    if (input$country != "Worldwide") {
      filtered_data <- filtered_data %>% filter(country_name == input$country)
    }
    
#Merge filtered data with the shapefile:
#Combines the data from Shapefile with the fertility data by matching
#Thus the country polygons also contain the fertility rate for the selected year
    world_data <- left_join(world, filtered_data, by = c("NAME" = "country_name"))
    
#Update the map with filtered data:
#Data is updated without reloading the page
    leafletProxy("map", data = world_data) %>%
#removes old map layers
      clearShapes() %>%
      clearControls() %>%
#Countries will be colored based on their fertility rate
      addPolygons(
        fillColor = ~colorNumeric(
          palette = c("#deebf7", "#08306b"),
          domain = world_data$Value,
          na.color = "white"
        )(Value),
#Contour lines are determined in their width
        weight = 0.5,
        opacity = 1,
        color = "black",
        dashArray = "3",
#Controls transparency
        fillOpacity = 0.7,
#Displays the country name and fertility rate on hover       
label = ~paste0(NAME, ": ", Value)
      ) %>%
#creates a legend with the color gradation of the fertility rate
      addLegend(
        pal = colorNumeric(
          palette = c("#deebf7", "#08306b"),
          domain = world_data$Value,
          na.color = "white"
        ),
        values = ~Value,
        title = "Rate",
        position = "bottomright"
      )
  })
}
#Run the app:
#Starts the programm and make the diagram functional
shinyApp(ui = ui, server = server)