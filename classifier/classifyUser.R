
library(RTextTools)
library(tm)
library(ranger)

setwd("../collection")
source("cache.R")
read.caches()
setwd("../classifier")

test.classifications <- data.frame(
    userid=character(0),
    daclass=character(0),
    prob=numeric(0))
#rf.model <- NULL
#docs <- NULL
#user.content <- NULL
#classes <- NULL
#whole.set <- NULL

# Return a one-row data frame for the userid passed, containing the userid,
# the predicted classification (daclass) and the confidence (prob).
#
# if userid is NULL, choose a random target to classify.
#
# if regenerate is FALSE, build the entire classifier instead of using
# previous cached results.
#
auto.classify <- function(userid=NULL, regenerate=TRUE) {

    rf.model <- build.classifier(regenerate)

    test.classifications <<- data.frame(
        userid=whole.set$userid[(training.size+1):length(docs)],
        daclass=predict(rf.model,test.data[,-ncol(test.data)])$predictions,
        prob=.5,
        stringsAsFactors=FALSE)

    if (is.null(userid)) {
        userid <- sample_n(test.classifications,1)$userid
    }

    return(test.classifications[test.classifications$userid==userid,])
}


# Return a (random forest) model for the data.
#
# if regenerate is FALSE, build the entire classifier instead of using
# previous cached results.
#
build.classifier <- function(regenerate=FALSE, use.real.data=TRUE,
    classification=1) {

cat("bc1\n")
    user.content <<- tweets.cache %>% group_by(userid) %>%
        summarize(content=paste(tweet, collapse=" "))

cat("bc2\n")
    class.column.name <- paste0("daclass",classification)
    whole.set <<- left_join(user.content,
        dplyr::select(training.cache, -timest))

        if (use.real.data) {
            classes <<- whole.set[[class.column.name]]
        } else {
            classes <<- 
                ifelse(str_detect(whole.set$content, "(?i)trump"),"C","L")
        }

cat("bc3\n")
    if (regenerate || !exists("docs")) {
        cat("regenerating docs\n")
        docs <<- whole.set$content

        #docs <- iconv(docs, sub="")
        docs <<- gsub("[^\x20-\x7E]", "", docs)
        docs <<- gsub("[^a-z]"," ",tolower(docs))

        docs <<- gsub("\\s+"," ",docs)
        docs <<- gsub("(^\\s)|(\\s$)","",docs)
    }

cat("bc4\n")
    training.size <- sum(!is.na(classes))

cat("bc5\n")
    if (regenerate || !exists("dtm")) {
        cat("regenerating dtm\n")
        dtm0 <- DocumentTermMatrix(Corpus(VectorSource(docs)),
           control=list(weighting=weightTfIdf,
                        removePunctuation=FALSE,
                        removeDigits=FALSE,
                        stemming=FALSE,
                        wordLengths=c(3,Inf)))
        dtm <<- removeSparseTerms(dtm0,.99)
    }

cat("bc6\n")
    training.data <- cbind(as.data.frame(as.matrix(dtm)[!is.na(classes),]),
            daclass=as.factor(classes)[!is.na(classes)])

cat("bc7\n")
    test.data <- cbind(as.data.frame(as.matrix(dtm)[is.na(classes),]),
            daclass=as.factor(classes)[is.na(classes)])

cat("bc8\n")
    rf.model <<- ranger(daclass ~ .,
       data=training.data,
       importance='impurity',
       write.forest=TRUE)

cat("bc9\n")
    return(rf.model)
}
