# Load necessary package
library(shiny)

# UI
ui <- fluidPage(
  titlePanel("Laboratory Reagent Preparation Calculator"),
  sidebarLayout(
    sidebarPanel(
      h3("Calculator"),
      selectInput("calc_type", "Choose Calculation:", 
                  choices = c("Serial Dilution", "Stock Solution Dilution")),
      
      # Inputs for Serial Dilution
      conditionalPanel(
        condition = "input.calc_type == 'Serial Dilution'",
        numericInput("initial_conc", "Initial Concentration (C1):", 1),
        numericInput("final_conc", "Final Concentration (C2):", 0.1),
        numericInput("dilution_factor", "Dilution Factor:", 10),
        numericInput("total_volume", "Total Volume (mL):", 10)
      ),
      
      # Inputs for Stock Solution Dilution
      conditionalPanel(
        condition = "input.calc_type == 'Stock Solution Dilution'",
        numericInput("stock_conc", "Stock Concentration (C1):", 10),
        numericInput("desired_conc", "Desired Concentration (C2):", 1),
        numericInput("final_volume", "Final Volume (mL):", 10)
      )
    ),
    
    mainPanel(
      h3("Results"),
      textOutput("result")
    )
  )
)

# Server logic
server <- function(input, output) {
  output$result <- renderText({
    if (input$calc_type == "Serial Dilution") {
      V1 <- (input$final_conc * input$total_volume) / input$initial_conc
      paste("Transfer", round(V1, 2), "mL of stock solution.")
    } else if (input$calc_type == "Stock Solution Dilution") {
      V1 <- (input$desired_conc * input$final_volume) / input$stock_conc
      paste("Transfer", round(V1, 2), "mL of stock solution.")
    }
  })
}

# Launch the app
shinyApp(ui = ui, server = server)
