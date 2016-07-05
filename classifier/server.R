
library(shiny)
require(RMySQL)

source("classifyUser.R")

read.caches()

shinyServer(function(input,output,session) {

    output$tweets <- renderUI({
        input$classify
        userid.to.classify <<- dplyr::sample_n(
            collect(anti_join(select(tweets.cache,userid), 
                select(training.cache,userid))),
            1)[[1]]
        tw3 <- collect(tweets.cache) %>% 
            dplyr::filter(userid==userid.to.classify) %>%
            summarize(content=paste(tweet, collapse="<br/><hr/>"))
        tw4 <- tw3[[1]]
        return(HTML(gsub("[^\x20-\x7E]", "", tw4)))
    })

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
    
})
