
library(RTextTools)

setwd("../collection")
source("cache.R")
read.caches()
setwd("../classifier")

# Return a one-row data frame for the userid passed, containing the userid,
# the predicted classification (FORESTS_LABEL) and the confidence
# (FORESTS_PROB).
#
# if userid is NULL, choose a random target to classify.
auto.classify <- function(userid=NULL, regenerate=FALSE) {

    if (!exists("test.classifications") || regenerate) {
        user.content <- collect(tweets.cache) %>% group_by(userid) %>%
            summarize(content=paste(tweet, collapse=" "))

        whole.set <- left_join(user.content,
            collect(dplyr::select(training.cache, userid, class))) %>% 
            arrange(class)
        classes <- whole.set$class
        docs <- whole.set$content

        #docs <- iconv(docs, sub="")
        docs <- gsub("[^\x20-\x7E]", "", docs)
        docs <- gsub("[^a-z]"," ",tolower(docs))

        docs <- gsub("\\s+"," ",docs)
        docs <- gsub("(^\\s)|(\\s$)","",docs)

        dtm <- create_matrix(docs)

        training.size <- sum(!is.na(classes))

        container <- create_container(dtm, classes, trainSize=1:training.size,
            testSize=(training.size+1):length(docs), virgin=TRUE)

        rf.model <- train_model(container,"RF")
        test.classifications <<- cbind(
            userid=whole.set$userid[(training.size+1):length(docs)],
            classify_model(container, rf.model),
            stringsAsFactors=FALSE)
    }

    if (is.null(userid)) {
        userid <- sample_n(test.classifications,1)$userid
    }

    return(test.classifications[test.classifications$userid==userid,])
}

