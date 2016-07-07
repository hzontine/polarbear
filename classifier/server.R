
library(shiny)
require(RMySQL)
require(xtable)

source("classifyUser.R")

shinyServer(function(input,output) {

    userid.to.classify <- "nothingClassifiedYet"

    output$trainingtweets <- renderUI({
        input$classify
        isolate({
            if (userid.to.classify != "nothingClassifiedYet") {
                cat("Record ", input$classification, " for user ",
                    userid.to.classify, "\n", sep="")
                conn <- get.connection(TRUE)
                dbGetQuery(conn,
                    paste0("insert into training values (",
                    "'",userid.to.classify,"','",
                    input$classification,"','",
                    input$classification2,"','",
                    input$classification3,"','",
                    input$classification4,"',now())")
                )
                dbDisconnect(conn)
            }
            userid.to.classify <<- dplyr::sample_n(
                setdiff(unique(select(tweets.cache,userid)), 
                    unique(collect(select(training.cache,userid)))),
                1)[[1]]
            cat("We are going to classify ", userid.to.classify, "next.\n")
            screenname <- collect(screennames.cache %>% 
                    dplyr::filter(userid==userid.to.classify) %>%
                    dplyr::select(screenname))[[1]][1]
            if (is.na(screenname) || length(screenname) != 1) {
                output$userid <- renderText({
                    paste0("<b>Twitter user #",userid.to.classify,"</b>")
                })
            } else {
                output$userid <- renderText({
                    paste0("<b>Twitter user #", userid.to.classify, " (@",
                        screenname,")</b>")
                })
            }
        })
            return(get.tweets.page.for(userid.to.classify))
    })

    get.tweets.page.for <- function(the.userid) {
        tw3 <- tweets.cache %>% 
            dplyr::filter(userid==the.userid) %>%
            summarize(content=paste(tweet, collapse="<br/><hr/>"))
        tw4 <- tw3[[1]]
        return(HTML(gsub("[^\x20-\x7E]", "", tw4)))
    }

    output$prediction <- renderText({
        return("<b>under construction</b>")
    })

    observeEvent(input$test, {
        prediction <- auto.classify(regenerate=FALSE)
        output$prediction <- renderUI(paste0(prediction$userid,
            " => ",prediction$class))
        output$testtweets <- renderUI(get.tweets.page.for(
            as.numeric(prediction$userid)))
    })


    output$topterms <- renderTable({
        if (input$analyze > 0) {
          withProgress(message="Please wait...", value=0, min=0, max=1, {
            isolate({
                output$waitmessage <<- renderText({
                    return("Please wait...")
                })
                my.rf.model <- build.classifier(use.real.data=TRUE,
                    classification=input$classificationanalyze)
                imps <- sort(my.rf.model$variable.importance, decreasing=TRUE)
                topterms <- imps[imps > 0]
                output$waitmessage <<- renderText({
                    return("Done!")
                })
                return(xtable(data.frame(importance=topterms)))
            })
          })
        }
    })
})
