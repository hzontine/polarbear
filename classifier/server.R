
library(shiny)
require(RMySQL)

source("classifyUser.R")

shinyServer(function(input,output,session) {

    output$trainingtweets <- renderUI({
        input$classify
        userid.to.classify <<- dplyr::sample_n(
            collect(anti_join(select(tweets.cache,userid), 
                select(training.cache,userid))),
            1)[[1]]
        return(get.tweets.page.for(userid.to.classify))
    })

    get.tweets.page.for <- function(the.userid) {
        tw3 <- collect(tweets.cache) %>% 
            dplyr::filter(userid==the.userid) %>%
            summarize(content=paste(tweet, collapse="<br/><hr/>"))
        tw4 <- tw3[[1]]
        return(HTML(gsub("[^\x20-\x7E]", "", tw4)))
    }

    observeEvent(input$classify, {
        isolate({
            cat("Record ", input$classification, " for user ",
                userid.to.classify, "\n", sep="")
            conn <- get.connection(TRUE)
            dbGetQuery(conn,
                paste0("insert into training values (",
                "'",userid.to.classify,"','",input$classification,"',now())")
            )
            dbDisconnect(conn)
        })
    })
    
    observeEvent(input$test, {
        prediction <- auto.classify()
        output$prediction <- renderUI(paste0(prediction$userid,
            " => ",prediction$FORESTS_LABEL))
        output$testtweets <- renderUI(get.tweets.page.for(
            as.numeric(prediction$userid)))
    })
})
