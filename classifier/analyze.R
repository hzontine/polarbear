
library(RTextTools)
library(tm)
library(ranger)
library(stringr)
library(dplyr)


load("trainingData.RData")

# Environment should now have "training.data" variable in it, which is a data
# frame that has one row per Twitter user with the following columns:
#
# userid -- Twitter userid
# content -- unprocessed content (concatenation of all tweets)
# daclass1 through daclass4 -- training classes for (1) ideology, (2)
# education, (3) gender, and (4) niceness.
#
# classification -- a number from 1-4, specifying a category, above
#
# dtm -- if this argument is provided, the doc-term matrix will not be
# renegerated.

build.classifier <- function(classification=1,dtm=NULL,data=training.data) {

    class.column.name <- paste0("daclass",classification)
    classes <- data[[class.column.name]]

    # Count "Not sure" as NA.
    classes <- ifelse(classes == "Not sure", NA, classes)

    if (is.null(dtm)) {
        dtm <- build.dtm(data)
    }

    training.size <- sum(!is.na(classes))

    training.only <- cbind(as.data.frame(as.matrix(dtm)[!is.na(classes),]),
            daclass=as.factor(classes)[!is.na(classes)])

    test.only <- cbind(as.data.frame(as.matrix(dtm)[is.na(classes),]),
            daclass=as.factor(classes)[is.na(classes)])

    cat("Building classifier...\n")
    rf.model <- ranger(daclass ~ .,
       data=training.only,
       importance='impurity',
       write.forest=TRUE)

    return(list(dtm=dtm,model=rf.model))
}

build.dtm <- function(data=training.data) {

    cat("Preprocessing tweets...\n")

    # Dave processing.
    docs <- data$content

    #docs <- iconv(docs, sub="")
    docs <- gsub("[^\x20-\x7E]", "", docs)
    cat("1 of 4...\n")
    docs <- gsub("[^a-z]"," ",tolower(docs))
    cat("2 of 4...\n")
    docs <- gsub("\\s+"," ",docs)
    cat("3 of 4...\n")
    docs <- gsub("(^\\s)|(\\s$)","",docs)
    cat("4 of 4.\n")

    cat("Generating doc-term matrix...\n")
    dtm0 <- DocumentTermMatrix(Corpus(VectorSource(docs)),
       control=list(weighting=weightTfIdf,
                    removePunctuation=FALSE,
                    removeDigits=FALSE,
                    stemming=FALSE,
                    wordLengths=c(3,Inf)))
    dtm <- removeSparseTerms(dtm0,.99)
    return(dtm)
}

assocs <- function(dtm=NULL) {
    if (is.null(dtm)) {
        dtm <- build.classifier(1)$dtm
    }
    term <- readline("Enter a term (or 'done'): ")
    while (term != "done") {
        associations <- findAssocs(dtm,term,rep(.05,1))[[1]]
        print(sort(associations,decreasing=TRUE)[
            1:(min(length(associations),20))])
        term <- readline("Enter a term (or 'done'): ")
    }
}
