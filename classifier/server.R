
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
                    unique(select(training.cache,userid))),
                1)[[1]]
            cat("We are going to classify ", userid.to.classify, "next.\n")
            output$userid <- renderUI({
                paste("userid ",userid.to.classify)
            })
            return(get.tweets.page.for(userid.to.classify))
        })
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

    output$waitmessage <- renderText({
        return("")
    })

    output$topterms <- renderTable({
        input$analyze
        isolate({
            my.rf.model <- build.classifier(use.real.data=TRUE,
                classification=input$classificationanalyze)
            imps <- sort(my.rf.model$variable.importance, decreasing=TRUE)
            topterms <- imps[imps > 0]
            return(xtable(data.frame(importance=topterms)))
        })
    })
})
