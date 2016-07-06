
require(RMySQL)
require(stringr)

setwd("../collection")
source("cache.R")
read.caches()
setwd("../classifier")

shakespeare.words <- c("hath","slain","doth","virtue","anon","apace","hither",
"forsooth","forswear","knave","methinks","perchance","prate","quoth","adorn")

teenager.words <- c("totally","fail","epic","omg","whatevs","yolo","legend",
"bro","swag","chillax","totes","bff","dab")

neutral.words <- c("donut","lamp","beer","hope","give","send","stand","kneel",
"bird","meet","come","have","normal","cold","bright","imagine","think","talk")

generate.test.data <- function() {

    NUM.SHAKESPEAREANS <- 100
    NUM.TEENAGERS <- 100

    conn <- get.connection(TRUE)

    create.fictitious.users(NUM.SHAKESPEAREANS, 1, neutral.words, 
        shakespeare.words, conn)
    create.fictitious.users(NUM.TEENAGERS, 101, neutral.words, teenager.words,
        conn)

    dbDisconnect(conn)
    
}

create.fictitious.users <- function(n, start.at, list1, list2, conn) {
    TWEETS.PER.USER <- 100
    for (i in 1:n) {
        for (j in 1:TWEETS.PER.USER) {
            tweet <- generate.tweet(list(list1,list2))
            dbGetQuery(conn,
                paste0("insert into tweets values (",
                    "'",i+(start.at-1),"','",tweet,"')")
            )
        }
    }
}

generate.tweet <- function(word.lists) {
    MIN.WORDS.PER.TWEET <- 3
    MAX.WORDS.PER.TWEET <- 30
    list.choices <- sample(c(1,2),
        sample(MIN.WORDS.PER.TWEET:MAX.WORDS.PER.TWEET),
        prob=c(.2,.8),replace=TRUE)
    tweet <- ""
    for (i in 1:length(list.choices)) {
        tweet <- paste(tweet,sample(word.lists[[list.choices[i]]],1))
    }
    return(str_trim(tweet))
}
