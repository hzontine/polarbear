library(shiny)
library(igraph)

shinyUI(fluidPage(
  
  titlePanel("Hidden Trump Effect"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("numAgents",
                  "# of agents:",
                  min = 4,
                  max = 80,
                  value = 16,
                  step = 4),
      sliderInput("probConnected",
                  "Probability of connection between Agents:",
                  min = 0.01,
                  max = 1.00,
                  value = 0.5),
      numericInput("numEncounters", label = h4("# of Encounters"), value = 2000),
      sliderInput("probUpdate",
                  "Probability of an Agent to update opinion:",
                  min = 0.01,
                  max = 1.0,
                  value = 0.5),
      sliderInput("probInternalize",
                  "Probability of internalizing opinion:",
                  min = 0.01,
                  max = 1.0,
                  value = 0.5),
      
      sliderInput("probPressure",
                  "Probability of giving in to peer pressure:",
                  min = 0.01,
                  max = 1.0,
                  value = 0.5),
      selectInput("terminate", label=h4("Terminate after uniformity of opinion"), choices = list("Hidden" = "hidden", "Expressed" = "expressed", "Both" = "both")),
      selectInput("victim", label=h4("A is victim?"), choices = list("True" = TRUE, "False" = FALSE)),
      selectInput("chooseRandomly", label=h4("Choose randomly?"), choices = list("Yes" = TRUE, "No" = FALSE)),
      
      submitButton("Submit")
    ),
    
    mainPanel(
      plotOutput("binaryPlot"),
      plotOutput("polarPlot")
    )
  )
))