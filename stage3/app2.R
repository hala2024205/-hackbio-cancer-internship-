# Complete code for CHOLDAT APP

\# Load necessary libraries

library(shiny)

library(shinydashboard)

library(ggplot2)

library(dplyr)

library(DT)

library(plotly)

library(readxl)

library(sf)

library(renv)

library(cachem)

library(plotly)

library(dplyr)

library(httr)

library(curl)

library(writexl)

library(wordcloud)

library(RColorBrewer)

library(orca)

library(viridis)

library(maps)

library(rmapshaper)

\# Load the data

\#setwd("C:/Users/musah yussif/Downloads/HackBio/stage-3/dataset on cholera")

reported\_cases <- read.csv("reported\_cases.csv")

reported\_deaths <- read.csv("deaths.csv")

fatality\_rate <- read.csv("fatality\_rate.csv")

\# Convert columns to numeric

reported\_cases$Number.of.reported.cases.of.cholera <- as.numeric(as.character(reported\_cases$Number.of.reported.cases.of.cholera))

reported\_deaths$Number.of.reported.deaths.from.cholera <- as.numeric(as.character(reported\_deaths$Number.of.reported.deaths.from.cholera))

fatality\_rate$Cholera.case.fatality.rate <- as.numeric(as.character(fatality\_rate$Cholera.case.fatality.rate))

\# UI

ui <- dashboardPage(

  dashboardHeader(title = "Cholera Dashboard"),

  dashboardSidebar(

    sidebarMenu(

      menuItem("Home", tabName = "home", icon = icon("home")),

      menuItem("Map", tabName = "map", icon = icon("globe")),

      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),

      menuItem("Top 10", tabName = "top10", icon = icon("chart-bar")),

      menuItem("Trends", tabName = "trends", icon = icon("line-chart")),

      menuItem("Summary", tabName = "summary", icon = icon("table"))

      

    )

  ),

  dashboardBody(

    tabItems(

      # Home Page

      tabItem(

        tabName = "home",

        fluidPage(

          tags$head(

            tags$style(HTML("

              .hero-image {

                background-image: url('hero\_image.jpg');

                background-size: cover;

                background-position: center;

                height: 400px;

                color: white;

                text-align: center;

                display: flex;

                justify-content: center;

                align-items: center;

                flex-direction: column;

                padding: 20px;

              }

              .hero-image h1 {

                font-size: 3em;

                margin: 0;

                color: black;

              }

              .hero-image p {

                font-size: 1.5em;

                margin: 10px 0;

                color: black;

              }

              .container {

                max-width: 1200px;

                margin: 0 auto;

                padding: 20px;

              }

              .sidebar img {

                max-width: 100%;

                height: auto;

                border-radius: 8px;

                box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);

              }

              .sidebar h4 {

                font-size: 2em;

                margin: 20px 0 10px;

                color: #fbfcfc;

              }

              .sidebar h5 {

                font-size: 1.2em;

                color: #bfc9ca;

              }

              .sidebar {

                padding: 20px;

                background-color: #4d5656;

                border-radius: 8px;

                box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);

              }

            "))

          ),

          div(class = "hero-image",

              h1("Welcome to CHOLDAT"),

              p("CHOLDAT is a comprehensive Cholera Database designed to explore the prevalence of cholera across the world.")

          ),

          div(class = "container",

              sidebarLayout(

                sidebarPanel(

                  tags$div(

                    class = "sidebar",

                    tags$img(src = "cho\_p.jpeg", height = "150px", width = "150px"),

                    h4("What is CHOLDAT?"),

                    h5("CHOLDAT is a Cholera Database that provides insights into the prevalence and impact of cholera worldwide. Use this tool to explore and understand cholera trends interactively.")

                  )

                ),

                mainPanel(

                  h2("App Summary"),

                  fluidRow(

                    valueBoxOutput("total\_cases\_box", width = 4),  # Info box for total cases

                    valueBoxOutput("total\_deaths\_box", width = 4),  # Info box for total deaths

                    valueBoxOutput("fatality\_rate\_box", width = 4)   # Info box for fatality rate

                  )

                )

              )

          )

        )

      ),

      

      tabItem(tabName = "map",

              h2("Geographical Spread of Cholera Cases"),

              plotlyOutput("mapPlot")

      ),

      

      tabItem(tabName = "overview",

              h2("Cholera Overview"),

              selectInput("year\_selector", "Select Year:", choices = unique(reported\_cases$Year), selected = unique(reported\_cases$Year)\[1]),

              selectInput("country\_selector", "Select Country:", choices = unique(reported\_cases$Countries..territories.and.areas), selected = unique(reported\_cases$Countries..territories.and.areas)\[1]),

              tabsetPanel(

                tabPanel("Cases", plotOutput("cases\_plot")),

                tabPanel("Deaths", plotOutput("deaths\_plot")),

                tabPanel("Fatality Rate", plotOutput("fatality\_rate\_plot"))

              )

      ),

      tabItem(tabName = "trends",

              h2("Cholera Trends Over Time"),

              plotOutput("trendsPlot")

      ),

      tabItem(tabName = "summary",

              h2("Summary Statistics"),

              DTOutput("summaryTable")

      ),

      tabItem(tabName = "top10",

              h2("Top 10 Countries by Fatalities"),

              plotOutput("top10\_plot")

      )

    )

  )

)

\# Server

server <- function(input, output) {

  

  # Calculate summary statistics

  total\_cases <- reactive({

    sum(reported\_cases$Number.of.reported.cases.of.cholera, na.rm = TRUE)

  })

  

  total\_deaths <- reactive({

    sum(reported\_deaths$Number.of.reported.deaths.from.cholera, na.rm = TRUE)

  })

  

  fatality\_rate\_avg <- reactive({

    mean(fatality\_rate$Cholera.case.fatality.rate, na.rm = TRUE)

  })

  

  # Info boxes

  output$total\_cases\_box <- renderValueBox({

    valueBox(

      value = total\_cases(),

      subtitle = "Total Cases",

      icon = icon("users"),  # Updated icon

      color = "blue"

    )

  })

  

  output$total\_deaths\_box <- renderValueBox({

    valueBox(

      value = total\_deaths(),

      subtitle = "Total Deaths",

      icon = icon("skull"),  # Updated icon

      color = "red"

    )

  })

  

  output$fatality\_rate\_box <- renderValueBox({

    valueBox(

      value = round(fatality\_rate\_avg() \* 100, 2),  # Convert to percentage

      subtitle = "Average Fatality Rate (%)",

      icon = icon("heartbeat"),  # Updated icon

      color = "orange"

    )

  })

  

  # Map plot

  # Map plot

  output$mapPlot <- renderPlotly({

    world <- st\_as\_sf(maps::map("world", fill = TRUE, plot = FALSE))

    

    total\_cases\_country <- reported\_cases %>%

      group\_by(Countries..territories.and.areas) %>%

      summarise(total\_cases = sum(Number.of.reported.cases.of.cholera, na.rm = TRUE)) %>%

      rename(ID = Countries..territories.and.areas)

    

    world\_cases <- world %>%

      left\_join(total\_cases\_country, by = "ID")

    

    p <- ggplot(data = world\_cases) +

      geom\_sf(aes(fill = total\_cases), color = NA) +

      scale\_fill\_viridis\_c(option = "viridis", na.value = "grey50") +

      labs(title = "Geographical Spread of Cholera Cases",

           fill = "Total Cases") +

      theme\_minimal()

    

    ggplotly(p)

  })

  

  # Overview plots

  combined\_yearly <- reactive({

    req(input$year\_selector, input$country\_selector)

    reported\_cases %>%

      filter(Countries..territories.and.areas == input$country\_selector) %>%

      group\_by(Year) %>%

      summarise(total\_cases = sum(Number.of.reported.cases.of.cholera, na.rm = TRUE)) %>%

      left\_join(

        reported\_deaths %>%

          filter(Countries..territories.and.areas == input$country\_selector) %>%

          group\_by(Year) %>%

          summarise(total\_deaths = sum(Number.of.reported.deaths.from.cholera, na.rm = TRUE)),

        by = "Year"

      ) %>%

      left\_join(

        fatality\_rate %>%

          group\_by(Year) %>%

          summarise(average\_fatality\_rate = mean(Cholera.case.fatality.rate, na.rm = TRUE)),

        by = "Year"

      )

  })

  

  output$cases\_plot <- renderPlot({

    ggplot(combined\_yearly(), aes(x = Year, y = total\_cases)) +

      geom\_line(color = "blue") +

      geom\_point(color = "blue", size = 2) +

      labs(title = "Total Cholera Cases Over Time", x = "Year", y = "Total Cases") +

      theme\_minimal() +

      theme(

        axis.line = element\_line(size = 1.2, color = "black"),

        axis.title.x = element\_text(face = "bold", size = 12),

        axis.title.y = element\_text(face = "bold", size = 12),

        plot.title = element\_text(face = "bold", size = 14, hjust = 0.5),

        panel.grid.major = element\_blank(),  # Remove major grid lines

        panel.grid.minor = element\_blank()   # Remove minor grid lines

      )

  })

  

  output$deaths\_plot <- renderPlot({

    ggplot(combined\_yearly(), aes(x = Year, y = total\_deaths)) +

      geom\_line(color = "red") +

      geom\_point(color = "red", size = 2) +

      labs(title = "Total Cholera Deaths Over Time", x = "Year", y = "Total Deaths") +

      theme\_minimal() +

      theme(

        axis.line = element\_line(size = 1.2, color = "black"),

        axis.title.x = element\_text(face = "bold", size = 12),

        axis.title.y = element\_text(face = "bold", size = 12),

        plot.title = element\_text(face = "bold", size = 14, hjust = 0.5),

        panel.grid.major = element\_blank(),  # Remove major grid lines

        panel.grid.minor = element\_blank()   # Remove minor grid lines

      )

  })

  

  output$fatality\_rate\_plot <- renderPlot({

    ggplot(combined\_yearly(), aes(x = Year, y = average\_fatality\_rate \* 1000)) +

      geom\_line(color = "green") +

      geom\_point(color = "green", size = 2) +

      labs(title = "Average Fatality Rate Over Time (%)", x = "Year", y = "Fatality Rate (%)") +

      scale\_y\_continuous(sec.axis = sec\_axis(\~./1000, name = "Fatality Rate (%)")) +

      theme\_minimal() +

      theme(

        axis.line = element\_line(size = 1.2, color = "black"),

        axis.title.x = element\_text(face = "bold", size = 12),

        axis.title.y = element\_text(face = "bold", size = 12),

        plot.title = element\_text(face = "bold", size = 14, hjust = 0.5),

        panel.grid.major = element\_blank(),  # Remove major grid lines

        panel.grid.minor = element\_blank()   # Remove minor grid lines

      )

  })

  

  

  # Trends plot

  output$trendsPlot <- renderPlot({

    cases\_yearly <- reported\_cases %>%

      group\_by(Year) %>%

      summarise(total\_cases = sum(Number.of.reported.cases.of.cholera, na.rm = TRUE))

    

    deaths\_yearly <- reported\_deaths %>%

      group\_by(Year) %>%

      summarise(total\_deaths = sum(Number.of.reported.deaths.from.cholera, na.rm = TRUE))

    

    fatality\_yearly <- fatality\_rate %>%

      group\_by(Year) %>%

      summarise(average\_fatality\_rate = mean(Cholera.case.fatality.rate, na.rm = TRUE))

    

    combined\_yearly\_trends <- cases\_yearly %>%

      left\_join(deaths\_yearly, by = "Year") %>%

      left\_join(fatality\_yearly, by = "Year")

    

    ggplot() +

      geom\_line(data = combined\_yearly\_trends, aes(x = Year, y = total\_cases, color = "Total Cases")) +

      geom\_point(data = combined\_yearly\_trends, aes(x = Year, y = total\_cases, color = "Total Cases")) + # Add points for cases

      geom\_line(data = combined\_yearly\_trends, aes(x = Year, y = total\_deaths, color = "Total Deaths")) +

      geom\_point(data = combined\_yearly\_trends, aes(x = Year, y = total\_deaths, color = "Total Deaths")) + # Add points for deaths

      geom\_line(data = combined\_yearly\_trends, aes(x = Year, y = average\_fatality\_rate \* 1000, color = "Fatality Rate")) +

      geom\_point(data = combined\_yearly\_trends, aes(x = Year, y = average\_fatality\_rate \* 1000, color = "Fatality Rate")) + # Add points for fatality rate

      labs(title = "Cholera Trends Over Time", x = "Year", y = "Counts / Rate") +

      scale\_y\_continuous(sec.axis = sec\_axis(\~./1000, name = "Fatality Rate (%)")) +

      theme\_minimal() +

      theme(

        axis.line = element\_line(size = 1.2, color = "black"),

        axis.title.x = element\_text(face = "bold", size = 12),

        axis.title.y = element\_text(face = "bold", size = 12),

        plot.title = element\_text(face = "bold", size = 14, hjust = 0.5),

        panel.grid.major = element\_blank(),  # Remove major grid lines

        panel.grid.minor = element\_blank()   # Remove minor grid lines

      ) +

      scale\_color\_manual(name = "Legend", values = c("blue", "red", "green"))

  })

  

  

  # Summary table

  output$summaryTable <- renderDT({

    # Ensure the datasets are merged

    summary\_data <- reported\_cases %>%

      group\_by(Countries..territories.and.areas) %>%

      summarise(

        Total\_Cases = sum(Number.of.reported.cases.of.cholera, na.rm = TRUE),

        Total\_Deaths = sum(reported\_deaths$Number.of.reported.deaths.from.cholera, na.rm = TRUE),

        Avg\_Fatality\_Rate = mean(fatality\_rate$Cholera.case.fatality.rate, na.rm = TRUE)

      ) %>%

      arrange(desc(Total\_Cases))

    

    datatable(summary\_data, options = list(pageLength = 10, scrollX = TRUE))

  })

  

  # Top 10 plot

  output$top10\_plot <- renderPlot({

    top10\_data <- reported\_deaths %>%

      group\_by(Countries..territories.and.areas) %>%

      summarise(total\_deaths = sum(Number.of.reported.deaths.from.cholera, na.rm = TRUE)) %>%

      arrange(desc(total\_deaths)) %>%

      head(10)

    

    p <- ggplot(top10\_data, aes(x = reorder(Countries..territories.and.areas, total\_deaths), y = total\_deaths)) +

      geom\_bar(stat = "identity", fill = "steelblue") +

      coord\_flip() +

      labs(title = "Top 10 Countries by Cholera Fatalities", x = "Country", y = "Total Fatalities") +

      theme\_minimal() +

      theme(

        axis.line = element\_line(size = 1.2, color = "black"),

        axis.title.x = element\_text(face = "bold", size = 12),

        axis.title.y = element\_text(face = "bold", size = 12),

        plot.title = element\_text(face = "bold", size = 14, hjust = 0.5),

        panel.grid.major = element\_blank(),

        panel.grid.minor = element\_blank()

      )

    

    # Adding labels outside the bars

    p + geom\_text(aes(label = total\_deaths), 

                  hjust = -0.1, # Adjust horizontal position

                  size = 4, 

                  color = "black") 

  })

  

  

 

}

\# Run the application 

shinyApp(ui = ui, server = server)
