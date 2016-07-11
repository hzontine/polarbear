
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

build.classifier <- function(classification=1,dtm=NULL) {

    class.column.name <- paste0("daclass",classification)
    classes <- training.data[[class.column.name]]

    # Count "Not sure" as NA.
    classes <- ifelse(classes == "Not sure", NA, classes)

    if (is.null(dtm)) {

        cat("Preprocessing tweets...\n")

        # Dave processing.
        docs <- training.data$content

        #docs <- iconv(docs, sub="")
        docs <- gsub("[^\x20-\x7E]", "", docs)
        docs <- gsub("[^a-z]"," ",tolower(docs))
        docs <- gsub("\\s+"," ",docs)
        docs <- gsub("(^\\s)|(\\s$)","",docs)

        training.size <- sum(!is.na(classes))

        cat("Generating doc-term matrix...\n")
        dtm0 <- DocumentTermMatrix(Corpus(VectorSource(docs)),
           control=list(weighting=weightTfIdf,
                        removePunctuation=FALSE,
                        removeDigits=FALSE,
                        stemming=FALSE,
                        wordLengths=c(3,Inf)))
        dtm <- removeSparseTerms(dtm0,.99)
    }

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


assocs <- function() {
    class.stuff <- build.classifier(1)
    term <- readline("Enter a term (or 'done'): ")
    while (term != "done") {
        print(sort(
            findAssocs(classstuff$dtm,term,rep(.1,1))[[1]],
            decreasing=TRUE)[1:20])
        term <- readline("Enter a term (or 'done'): ")
    }
}
