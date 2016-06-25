
source("manualApi.R")
source("cache.R")

# To steal infrastructure methods (perform.cursor.call)
source("collectUserSet.R")   

library(methods)

Memento <- setRefClass("Memento",
    fields = list(
        i="numeric"
    ),
    methods = list(
        initialize=function(...) {
            .self$initFields(...)
            i <<- 1
        },
        set.i=function(new.i) {
            i <<- new.i
        }
    )
)

# Given a vector of Twitter IDs, get their most recent n-ish tweets, and store
# them as rows in the database.
#
# verbose -- if TRUE, print breadcrumb trail to the screen.
#
collect.recent.tweets <- function(ids, num.tweets=200, verbose=TRUE, 
    tweets.memento=NULL) {

    if (is.null(tweets.memento)) {
        # Don't store any progress globally.
        tweets.memento <- Memento$new()
    } else {
        if (tweets.memento$i > 1) {
            cat("Resuming at iteration ", tweets.memento$i, ".\n", sep="")
        }
    }

    read.caches()

    f <- dplyr::filter

    for (i in (tweets.memento$i):length(ids)) {

        tweets.memento$set.i(i)

        u <- ids[i]

        if (verbose) {
            cat("Fetching tweets for user ", u, " (", i, "/",
                length(ids), " = ", 
                round(i/length(ids)*100,1), "%)...\n", sep="")
        }

        tweets <- get.tweets.by.userid(u, num.tweets, verbose)
    }
}

get.tweets.by.userid <- function(userid, num.tweets=200, verbose=FALSE) {
    if (exists.in.cache(userid, tweets.cache, check.nodata=TRUE)) {
        if (verbose) {
            cat("Returning cached tweets for user ", userid, "...\n", sep="")
        }
        return(get.cached.values(userid, tweets.cache))
    }
    tweets <- perform.cursor.call(paste0(
        "https://api.twitter.com/1.1/statuses/user_timeline.json?user_id=",
            userid,"&count=",num.tweets,"&trim_user=true&include_rts=false"),
            "text",verbose)
    add.to.cache(userid, tweets, "tweets.cache")
    return(get.tweets.by.userid(userid, num.tweets, FALSE))
}


# Run this line only if you want to manually whack older results and start
# over.
# main.tweets.memento <- Memento$new()

main <- function(start.over=FALSE) {
    read.caches()
    users <- unique(collect(dplyr::select(screennames.cache, userid)))$userid
    if (start.over) {
        main.tweets.memento <<- Memento$new()
    }
    collect.recent.tweets(users, verbose=TRUE, 
        tweets.memento=main.tweets.memento)
    cat("All done.\n")
}
