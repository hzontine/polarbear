
library(RTextTools)

setwd("../collection")
source("cache.R")
read.caches()
setwd("../classifier")

#tweets <- collect(tweets.cache) %>% group_by(userid) %>%
#    summarize(content=paste(tweet, collapse=" "))
#
#classes <- c("red","blue","blue","red",rep(NA,nrow(tweets)-4))
#docs <- tweets$content
#
#docs <- iconv(docs, sub="")
#docs <- gsub("[^a-z]"," ",tolower(docs))
#
#docs <- gsub("\\s+"," ",docs)
#docs <- gsub("(^\\s)|(\\s$)","",docs)
#dtm <- create_matrix(docs)
#
#training.size <- length(docs)
#
#container <- create_container(dtm, classes, trainSize=1:4,
#    testSize=5:length(docs), virgin=FALSE)
#
#rf.model <- train_model(container,"RF")
#rf.classify <- classify_model(container, rf.model)
#
#results <- cbind(rf.classify, doc=docs)
#print(results)
