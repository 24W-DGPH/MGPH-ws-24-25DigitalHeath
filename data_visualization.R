#Loading the required R-packages:
#Again a help to process the data, this time it was supplemented with packages that are used for visualization.
pacman::p_load(
  rio,       # import/export
  here,      # filepaths
  lubridate, # working with dates
  plotly,    # interactive plots
  scales,    # quick percents
  tidyverse,  # data management and visualization
  shiny,
  ggplot2) 

#Read in and prepare data:
#The data was read in as csv files for further processing.
data_long <- read_csv("clean_data.csv")
statistics <- read_csv("statistics.csv")

##Create Shiny app
#On the one hand, a flexible web interface is created and a title for the whole.
ui <- fluidPage(
  titlePanel("Fertility Rate"),

#Create layout with slidebar:
#Creates a two-column structure, on the sidebar things to enter and a main area in which the selected things are displayed.
#There are two selection options for different countries, but it is also possible to select none. In addition, the previously created median, mean, minimum and maximum can also be displayed if desired. Finally, the period from 1960 to 2020 can be selected.
  sidebarLayout(
    sidebarPanel(
      selectInput("country1", "Choose the first country:", 
                  choices = c("Nothing selected", unique(data_long$country_name)), 
                  selected = "Nothing selected"),
      
      selectInput("country2", "Choose the second country:", 
                  choices = c("Nothing selected", unique(data_long$country_name)), 
                  selected = "Nothing selected"),
      
      selectInput("agg_function", "Choose worldwide statistic:",
                  choices = list("Nothing selected",
                                 "Median", 
                                 "Mean", 
                                 "Minimum", 
                                 "Maximum"),
                  selected = "Nothing selected"),
      
      selectInput("year_start", "Select start year:",
                  choices = unique(data_long$Year), 
                  selected = min(data_long$Year)),
      
      selectInput("year_end", "Select end year:",
                  choices = unique(data_long$Year), 
                  selected = max(data_long$Year))
    ),
    mainPanel(
      plotOutput("countryPlot")
    )
  )
)

#Server function for data processing and plot creation:
#In the server, the logic has the function of converting into plots.
server <- function(input, output) {

#A chart is generated based on the user data entered.
    output$countryPlot <- renderPlot({

#The filter only selects data for the selected countries, period, ect. If no other country is selected, it is ignored. Missing values are removed and the data is re-sorted.    
      filtered_data <- data_long %>% 
      filter(
        country_name %in% c(input$country1, 
                            if (input$country2 != "Nothing selected") input$country2 else NULL),
        Year >= input$year_start,
        Year <= input$year_end
      ) %>%
      drop_na(Value) %>% 
      arrange(country_name, Year)

#Only the global statistics values for the selected time period are displayed in this filter. If nothing is selected, it remains empty. Missing values are removed and the data is sorted.         
        filtered_statistics <- statistics %>% 
      filter(
        Statistic %in% c(input$agg_function),
        Year >= input$year_start,
        Year <= input$year_end
      ) %>%
      drop_na(Value) %>% 
      arrange(Statistic, Year)

#The statistics data is given a new name in the column to distinguish it from the countries. And the original statistics column is removed.
    filtered_statistics <- filtered_statistics %>%
      mutate(country_name = paste("Global", input$agg_function, sep = " - ")) %>%
      select(-Statistic)
    
#Both data sets are combined
    combined_data <- bind_rows(filtered_data, filtered_statistics)

#Create diagram:
#Creates a diagram and describes which values lie on which axis. It is described that the points relate to lines. The title and the axes are also labelled and a modern design is used.
    ggplot(combined_data, aes(x = Year, y = Value, color = country_name)) +
      geom_line(size = 1) +
      geom_point() +
      labs(title = "Fertility Rate of 186 Countries in 1960-2020",
           x = "Year", 
           y = "Value", 
           color = "Country") +
      theme_minimal()
  })
}

#Start app:
#This connects the Ui and server and starts the Shiny app.
shinyApp(ui = ui, server = server)