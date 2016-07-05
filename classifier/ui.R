
library(shiny)

shinyUI(fluidPage(

    tags$head(tags$link(rel="stylesheet", type="text/css", href="shiny.css")),

    titlePanel("Ideology classifier"),

    tabsetPanel(
        tabPanel("Train",
            radioButtons("classification","Classify user",
                list("Liberal","Conservative","Unsure"), inline=TRUE),
            actionButton("classify","Classify"),
            htmlOutput("tweets")
        ),
        tabPanel("Test",
            h3(textOutput("test"))
        )
    )
))
