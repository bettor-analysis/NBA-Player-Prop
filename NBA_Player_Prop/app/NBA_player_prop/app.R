# Load required packages
library(shiny)
library(readr)
library(dplyr)

# Create Shiny App
ui <- fluidPage(
  titlePanel("NBA Player Stats Simulation"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain,.csv")),
      numericInput("sim_games", "Number of Simulated Games:", 1000),
      actionButton("run", "Run Simulation"),
      actionButton("refresh", "Reset")
    ),
    mainPanel(
      tableOutput("output_median")
    )
  )
)

server <- function(input, output) {
  
  data_to_simulate <- reactiveVal(NULL) # Initialize a reactive value to NULL
  
  observeEvent(input$run, {
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }
    
    player_data <- read_csv(inFile$datapath)
    player_data <- player_data %>% filter(!is.na(G))
    
    stats <- player_data %>% summarise(across(c(PTS, AST, STL, TRB, BLK, TOV, PF), list(median = median, sd = sd)))
    
    sim_data <- data.frame(
      PTS = rnorm(input$sim_games, mean = stats$PTS_median, sd = stats$PTS_sd),
      AST = rnorm(input$sim_games, mean = stats$AST_median, sd = stats$AST_sd),
      STL = rnorm(input$sim_games, mean = stats$STL_median, sd = stats$STL_sd),
      TRB = rnorm(input$sim_games, mean = stats$TRB_median, sd = stats$TRB_sd),
      BLK = rnorm(input$sim_games, mean = stats$BLK_median, sd = stats$BLK_sd),
      TOV = rnorm(input$sim_games, mean = stats$TOV_median, sd = stats$TOV_sd),
      PF = rnorm(input$sim_games, mean = stats$PF_median, sd = stats$PF_sd)
    )
    
    sim_data_median <- round(summarise(sim_data, across(everything(), median)), 1)
    data_to_simulate(sim_data_median) # Set the reactive value
  })
  
  observeEvent(input$refresh, {
    data_to_simulate(NULL) # Reset to NULL or to new values
  })
  
  output$output_median <- renderTable({
    data_to_simulate() # Get the reactive value
  })
}

# Run the app
shinyApp(ui, server)
