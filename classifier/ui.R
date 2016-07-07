
library(shiny)

shinyUI(fluidPage(

    tags$head(tags$link(rel="stylesheet", type="text/css", href="shiny.css")),

    titlePanel("Ideology classifier"),

    tabsetPanel(selected="Train",
        tabPanel("Train",
            sidebarLayout(sidebarPanel(
                radioButtons("classification","Classify user",
                    list("Liberal","Conservative","Not sure"), inline=TRUE),
                radioButtons("classification2","",
                    list("Educated","Uneducated","Not sure"), inline=TRUE),
                radioButtons("classification3","",
                    list("Optimistic","Pessimistic","Not sure"), inline=TRUE),
                radioButtons("classification4","",
                    list("Nice","Jerk","Not sure"), inline=TRUE),
                actionButton("classify","Classify")
            ),
            mainPanel(
                htmlOutput("userid"),
                hr(),
                htmlOutput("trainingtweets")
            )
        )),
#        tabPanel("Test",
#            actionButton("test","Show auto-classification"),
#            htmlOutput("prediction")
#            htmlOutput("testtweets")
#        ),
        tabPanel("Analyze",
            radioButtons("classificationanalyze","Choose:",
                list("Ideology"=1,"Education"=2,"Optimistic-ness"=3,
                "Niceness"=4), inline=TRUE),
            actionButton("analyze","Show top terms"),
            htmlOutput("waitmessage"),
            htmlOutput("topterms"),
            h3("Calculating...please be patient...")
        )
    )
))
