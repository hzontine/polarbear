
library(shiny)

shinyUI(fluidPage(

    tags$head(tags$link(rel="stylesheet", type="text/css", href="shiny.css")),

    titlePanel("Ideology classifier"),

    tabsetPanel(
        tabPanel("Train",
            radioButtons("classification","Classify user",
                list("Liberal","Conservative","Unsure"), inline=TRUE),
            actionButton("classify","Classify"),
            htmlOutput("trainingtweets")
        ),
        tabPanel("Test",
            actionButton("test","Show auto-classification"),
            htmlOutput("prediction"),
            htmlOutput("testtweets")
        )
    )
))
