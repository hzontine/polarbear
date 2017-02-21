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

      sliderInput("probUpdate",
                  HTML("Probability of updating internal opinion (online):<br/><code>prob.update</code>"),
                  min = 0,
                  max = 1.0,
                  value = 0.5),
      sliderInput("probPressure",
                  HTML("Probability of \"giving in\" & changing expressed (face2face):<br/><code>prob.knuckle.under.pressure</code>"),
                  min = 0,
                  max = 1.0,
                  value = 0.5),
      sliderInput("probInternalize",
                  HTML("Probability of internalizing expressed opinion (face2face):<br/><code>prob.internalize.expressed.opinion</code>"),
                  min = 0,
                  max = 1.0,
                  value = 0.5),
      sliderInput("probConnected",
                  "Probability of connection between Agents:",
                  min = 0.01,
                  max = 1.00,
                  value = 0.5),
      
      fluidRow(
        column(4, selectInput("terminate", label=h5("Terminate after uniformity"), choices = list("Hidden" = "hidden", "Expressed" = "expressed", "Both" = "both", "Never terminate" = "never"))),
        column(4, selectInput("victim", label=h5("A is victim?"), choices = list("True" = TRUE, "False" = FALSE))),
        column(4, selectInput("chooseRandomly", label=h5("Choose randomly?"), choices = list("Yes" = TRUE, "No" = FALSE)))
      )
      
    ),
    
    mainPanel(
      plotOutput("binaryPlot"),
      plotOutput("polarPlot"),
      plotOutput("biasPlot"),
      plotOutput("hiddenPlot"),
      plotOutput("expPlot")
    )
  )
))
