
library(RTextTools)
library(tm)
library(ranger)

setwd("../collection")
source("cache.R")
read.caches()
setwd("../classifier")


# Return a one-row data frame for the userid passed, containing the userid,
# the predicted classification (class) and the confidence (prob).
#
# if userid is NULL, choose a random target to classify.
#
# if regenerate is FALSE, build the entire classifier instead of using
# previous cached results.
#
# if rtt is TRUE, use RTextTools package. Otherwise, use tm and ranger.
#
auto.classify <- function(userid=NULL, regenerate=TRUE, rtt=FALSE) {

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

        training.size <- sum(!is.na(classes))

      if (rtt) {
        dtm <- create_matrix(docs)

        container <- create_container(dtm, classes, trainSize=1:training.size,
            testSize=(training.size+1):length(docs), virgin=TRUE)

        rf.model <- train_model(container,"RF")
        test.classifications <<- cbind(
            userid=whole.set$userid[(training.size+1):length(docs)],
            classify_model(container, rf.model),
            stringsAsFactors=FALSE)
        names(test.classifications) <<- c("userid","class","prob")
      } else {
        dtm0 <- DocumentTermMatrix(Corpus(VectorSource(docs)),
           control=list(weighting=weightTfIdf,
                        removePunctuation=FALSE,
                        removeDigits=FALSE,
                        stemming=FALSE,
                        wordLengths=c(3,Inf)))
        dtm <- removeSparseTerms(dtm0,.999)

        training.data <- cbind(as.data.frame(as.matrix(dtm)[!is.na(classes),]),
                class=as.factor(classes)[!is.na(classes)])

        test.data <- cbind(as.data.frame(as.matrix(dtm)[is.na(classes),]),
                class=as.factor(classes)[is.na(classes)])

        rf <- ranger(class ~ .,
           data=training.data,
           importance='impurity',
           write.forest=TRUE)

        test.classifications <<- data.frame(
            userid=whole.set$userid[(training.size+1):length(docs)],
            class=predict(rf,test.data[,-ncol(test.data)])$predictions,
            prob=.5,
            stringsAsFactors=FALSE)
      }
    }

    if (is.null(userid)) {
        userid <- sample_n(test.classifications,1)$userid
    }

    return(test.classifications[test.classifications$userid==userid,])
}

