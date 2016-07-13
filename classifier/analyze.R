
library(RTextTools)
library(tm)
library(ranger)
library(stringr)
library(dplyr)
library(e1071)



main <- function() {

    # Load "training.data". This is a data frame with the following columns:
    # . userid -- Twitter userid 
    # . content -- unprocessed content (concatenation of all tweets)
    # . daclass1 through daclass4 -- training classes for (1) ideology,
    #   (2) education, (3) gender, and (4) niceness.
    # Note the "daclass" columns all have one or more NAs, and also one or 
    # more "Not sure" entries.
    load("trainingData.RData")   

    ideology.training <- dplyr::select(training.data, userid, content, 
        Class=daclass1)

    # Count "Not sure" as NA.
    ideology.training$Class <- as.factor(ifelse(
        str_count(ideology.training$Class, "Not sure") == 1,
        NA, ideology.training$Class))
    ideology.training <- ideology.training[!is.na(ideology.training$Class),]

    if (exists("save.dtm")) {
        classifier <<- build.classifier(ideology.training,big.tweets,save.dtm)
    } else {
        classifier <<- build.classifier(ideology.training,big.tweets)
    }

    cat("For user 2161408706, we predict:",
        predict.ideology(classifier, "2161408706"), "\n")

    cat("Predicting all ideologies...\n")
    ideos <- predict.all.ideologies(classifier)
    predicted.ideologies <<-
        cbind(collect(dplyr::select(classifier$dtm, Userid)), ideos)
}


predict.ideology <- function(classifier, userid) {
    prediction <- predict(classifier$model, 
        collect(dplyr::filter(classifier$dtm, Userid==userid)))$predictions
    return(classifier$model$forest$levels[prediction])
}


predict.all.ideologies <- function(classifier) {
    predictions <- predict(classifier$model, 
        collect(classifier$dtm))$predictions
    return(classifier$model$forest$levels[predictions])
}


# training.data: a data frame that has one row per labeled Twitter user with 
# the following columns:
# . userid -- Twitter userid 
# . content -- unprocessed content (concatenation of all tweets)
# . Class -- class label
#
# all.data: a data frame that has one row per Twitter user with the following 
# columns:
# . userid -- Twitter userid 
# . content -- unprocessed content (concatenation of all tweets)
#
# docterm -- if this argument is provided, the doc-term matrix will not be
# renegerated.
#
build.classifier <- function(training.data, all.data, docterm=NULL) {

    if (is.null(docterm)) {
        cat("Building document-term matrix...\n")
        docterm <- build.dtm(all.data$content)
        cat("Preparing training data...\n")
        dtmatrix <- data.frame(Userid=all.data$userid, 
            as.matrix(docterm), stringsAsFactors=FALSE)
    } else {
        dtmatrix <- docterm
    }

    real.training.data <- 
        inner_join(dtmatrix, dplyr::select(training.data, -content),
            by=c(Userid="userid"))

    cat("Building classifier...\n")
    rf.model <- ranger(Class ~ ., data=real.training.data,
       importance='impurity', write.forest=TRUE)

    a.sqlite.db <- src_sqlite("aSqliteDb.sqlite3", create=TRUE)
    dtmatrix.sqlite <- copy_to(a.sqlite.db, dtmatrix, indexes=list("Userid"))
    return(list(dtm=dtmatrix.sqlite,model=rf.model))
}

# Given a vector of text content, return a DocumentTermMatrix object for it.
build.dtm <- function(docs) {

    cat("Preprocessing tweets...\n")

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
    dtm <- removeSparseTerms(dtm0,.9)
    return(dtm)
}

# Allow the user to interactively explore co-occurrences. For each word the
# user enters, print the top 20 most commonly associated other words.
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

# Return a named vector of values, one for each word in the corpus. Each value
# is the ratio of the fraction of users in class 1 who used the word to the
# fraction in class 2. (I think.)
indicative.words <- function(classification=1,data=training.data) {

    class.column.name <- paste0("daclass",classification)
    classes <- data[[class.column.name]]

    # Count "Not sure" as NA.
    classes <- ifelse(str_count(classes, "Not sure") == 1, NA, classes)
    data <- data[!is.na(classes),-1]
    classes <- classes[!is.na(classes)]

    data <- data[,1:(ncol(data)-4)]

    dtm <- build.dtm(data)

    nb <- naiveBayes(as.matrix(dtm),classes)

    measures <- sapply(names(nb$tables), function(word) {
        table <- nb$tables[[word]]
        return(unname(table[1,1] / table[2,1]))
    })

    return(measures)
}
