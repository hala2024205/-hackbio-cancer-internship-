# Load necessary packages
#options(repos = c(CRAN = "https://cloud.r-project.org/"))
#install.packages("renv")
#renv::init()  # Initialize a new renv project
#renv::snapshot()  # Snapshot your project to lock the package versions

#install.packages("dplyr")
library(dplyr)

# Load the three CSV files
#death_cases <- read.csv("CHOLERA_death_cases.csv")
#fatality_cases <- read.csv("CHOLERA_fatality_cases.csv")
#reported_cases <- read.csv("CHOLERA_reported_cases.csv")

# Preview the first few rows of each dataset
#head(death_cases)
#head(fatality_cases)
#head(reported_cases)

# Check the column names of each dataset
colnames(combined_data)
#colnames(fatality_cases)
#colnames(reported_cases)

#combined_data <- death_cases %>%
## select(Year, Countries = Countries..territories.and.areas, Deaths = Number.of.reported.deaths.from.cholera) %>%
#  full_join(fatality_cases %>% select(Year, Countries = Countries..territories.and.areas, Fatality = Cholera.case.fatality.rate), 
#            by = c("Year", "Countries")) %>%
#  full_join(reported_cases %>% select(Year, Countries = Countries..territories.and.areas, Reported = Number.of.reported.cases.of.cholera), 
#            by = c("Year", "Countries"))

# Save the combined data
#write.csv(combined_data, "combined_cholera_data.csv", row.names = FALSE)

#set working directory
#setwd("C://Users//Faith AYOADE//Desktop//Desktop//HACKBIO//stage 3//The cholera outbreak//mychlolerashinyap2")
#getwd()

#remove the list in the enviroment
#rm(list = ls())
#load the csv file
combined_data <- read.csv("combined_cholera_data.csv")
# View the combined data in a new window
#View(combined_data)

# Check for missing values in the combined dataset
colSums(is.na(combined_data))

# View the data structure to confirm the types of each column
str(combined_data)

# Convert the 'Deaths' and 'Fatality' columns to numeric
combined_data$Deaths <- as.numeric(combined_data$Deaths)
combined_data$Fatality <- as.numeric(combined_data$Fatality)

# Check for any warnings (due to NAs generated during conversion)
combined_data[is.na(combined_data$Deaths), ]


# Basic summary of the data
summary(combined_data)

# Find rows where 'Deaths' conversion failed
non_numeric_deaths <- combined_data[is.na(combined_data$Deaths), ]

# Find rows where 'Fatality' conversion failed
non_numeric_fatality <- combined_data[is.na(combined_data$Fatality), ]

# Display the problematic rows for inspection
non_numeric_deaths
non_numeric_fatality
# Remove any non-numeric characters in 'Deaths' and 'Fatality'
combined_data$Deaths <- as.numeric(gsub("[^0-9.]", "", combined_data$Deaths))
combined_data$Fatality <- as.numeric(gsub("[^0-9.]", "", combined_data$Fatality))

# Check the structure again after cleaning
str(combined_data)

# Install required packages if not already installed
#install.packages(c("ggplot2", "leaflet", "shiny", "dplyr", "shinythemes"))


library(ggplot2)

# Create a line plot for reported cases over time
ggplot(combined_data, aes(x = Year, y = Reported, color = Countries)) +
  geom_line(size = 1) +
  labs(title = "Cholera Reported Cases Over Time",
       x = "Year", y = "Number of Reported Cases") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_viridis_d() # Use a vibrant color scale

# Bar plot showing deaths and fatality rates for each country
ggplot(combined_data, aes(x = Countries, y = Deaths, fill = Fatality)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cholera Deaths and Fatality Rates by Country",
       x = "Country", y = "Number of Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_gradient(low = "blue", high = "red") # Color gradient based on fatality rates

library(leaflet)

# Example dataframe with country coordinates (you'll need to merge this with your data)
country_coords <- data.frame(
  Countries = c("Afghanistan", "Bangladesh", "India"), # add more
  lat = c(33.93911, 23.684994, 20.593684),
  lon = c(67.709953, 90.356331, 78.96288)
)

# Merge country coordinates with combined_data
combined_data_map <- merge(combined_data, country_coords, by = "Countries")

# Create an interactive map
leaflet(data = combined_data_map) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat, 
                   radius = ~sqrt(Reported)/100, # Scale marker size by reported cases
                   color = ~ifelse(Deaths > 0, "red", "green"),
                   label = ~paste(Countries, "Reported Cases:", Reported, "<br>", "Deaths:", Deaths)) %>%
  addLegend(position = "bottomright", 
            title = "Reported Cases",
            colors = c("green", "red"),
            labels = c("No Deaths", "Deaths"))

#now to the shiny 

library(shiny)
library(ggplot2)
library(leaflet)

# Define UI for the Shiny app
ui <- fluidPage(
  #theme = shinytheme("cerulean"),
  
  titlePanel("Cholera Outbreak Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select a Country:",
                  choices = unique(combined_data$Countries), selected = "Afghanistan"),
      sliderInput("year_range", "Select Year Range:",
                  min = min(combined_data$Year), max = max(combined_data$Year),
                  value = c(min(combined_data$Year), max(combined_data$Year)),
                  step = 1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Reported Cases Over Time", plotOutput("linePlot")),
        tabPanel("Cholera Deaths by Country", plotOutput("barPlot")),
        tabPanel("Interactive Map", leafletOutput("choleraMap"))
      )
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output) {
  
  # Filtered dataset based on user input
  filtered_data <- reactive({
    combined_data %>%
      filter(Countries == input$country & Year >= input$year_range[1] & Year <= input$year_range[2])
  })
  
  # Line plot for reported cases over time
  output$linePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Year, y = Reported, color = Countries)) +
      geom_line(size = 1) +
      labs(title = "Cholera Reported Cases Over Time",
           x = "Year", y = "Number of Reported Cases") +
      theme_minimal()
  })
  
  # Bar plot for deaths by country
  output$barPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Countries, y = Deaths, fill = Fatality)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Cholera Deaths and Fatality Rates by Country",
           x = "Country", y = "Number of Deaths") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_gradient(low = "blue", high = "red")
  })
  
  # Interactive map
  output$choleraMap <- renderLeaflet({
    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(~lon, ~lat, 
                       radius = ~sqrt(Reported)/100,
                       color = ~ifelse(Deaths > 0, "red", "green"),
                       label = ~paste(Countries, "Reported Cases:", Reported, "<br>", "Deaths:", Deaths))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
