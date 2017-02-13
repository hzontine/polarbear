library(shiny)
library(igraph)

shinyUI(fluidPage(
  
  titlePanel("Hidden Trump Effect"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(3,
          actionButton("runsim", label="Run sim")),
        column(3,
          numericInput("numEncounters", label = h5("# of Encounters"), value = 2000)),
        column(6,
            radioButtons("seedType",label="Seed",
                choices=c("Random"="rand",
                    "Specific"="specific"),
                selected="rand",
                inline=TRUE),
            conditionalPanel(condition="input.seedType == 'specific'",
                numericInput("seed","",value=0))
            )
        ),
      selectInput("hte", label=h5("Hidden Trump Effect"), choices = list("On" = TRUE, "Off" = FALSE)),
      sliderInput("numAgents",
                  "# of agents:",
                  min = 4,
                  max = 80,
                  value = 64,
                  step = 4),
      sliderInput("probConnected",
                  "Probability of connection between Agents:",
                  min = 0.01,
                  max = 1.00,
                  value = 0.5),
      sliderInput("probPressure",
                  "Probability of giving in to peer pressure:",
                  min = 0.01,
                  max = 1.0,
                  value = 0.5),
      sliderInput("probUpdate",
                  "Probability of updating an opinion:",
                  min = 0.01,
                  max = 1.0,
                  value = 0.5),
      sliderInput("probInternalize",
                  "Probability of internalizing opinion:",
                  min = 0.01,
                  max = 1.0,
                  value = 0.5),
      fluidRow(
        column(4, selectInput("terminate", label=h5("Terminate after uniformity"), choices = list("Hidden" = "hidden", "Expressed" = "expressed", "Both" = "both", "Never terminate" = "never"))),
        column(4, selectInput("victim", label=h5("A is victim?"), choices = list("True" = TRUE, "False" = FALSE))),
        column(4, selectInput("chooseRandomly", label=h5("Choose randomly?"), choices = list("Yes" = TRUE, "No" = FALSE)))
      )
      
    ),
    
    mainPanel(
      plotOutput("binaryPlot"),
      plotOutput("polarPlot")
    )
  )
))
