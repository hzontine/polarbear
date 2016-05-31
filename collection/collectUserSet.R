
source("manualApi.R")


# Given a set of "seed" screen names S, return a data frame for a graph. This
# data frame will contain all users in a set U who satisfy the following
# criteria: (1) each user follows at least one element of S, and (2) each
# element follows at least one other element of U.
#
# The data frame will be in the form of an edge list, with column 1 being the
# follower and column 2 the followee.
#
collect.user.set <- function(S, verbose=TRUE) {

    if (verbose) {
        if (length(S) < 10) {
            cat("Collecting user set for seed set:", S, "...\n")
        } else {
            cat("Collecting user set for seed set of size ",
                length(S), "...\n", sep="")
        }
    }

    followers.of.S <- vector()
    for (s in S) {
        this.seed.nodes.followers <- get.followers.of(s)
        followers.of.S <- union(followers.of.S, this.seed.nodes.followers)
    }

    for (u in followers.of.S) {
    }

#    U.screen.names <- vector(length=length(U))
#    for (chunk.num in 1:ceiling(length(U)/100)) {
#        chunk.range <- (1+100*(chunk.num-1)):min(100*chunk.num,length(U))
#        U.screen.names[chunk.range] <- get.screen.names(U[chunk.range])
#    }
    return(U.screen.names)
}


get.followers.of <- function(screenname) {

    return(perform.cursor.call(paste0(
        "https://api.twitter.com/1.1/followers/ids.json?screen_name=",
            screenname),"ids"))
}

perform.cursor.call <- function(url, field.to.extract) {
    results.so.far <- vector()
    the.call <- make.manual.twitter.api.call(paste0(
        url, "&cursor=-1"))
    results.so.far <- union(results.so.far, the.call[[field.to.extract]])
    cursor <- the.call$next_cursor
    while (cursor != 0) {
        the.call <- make.manual.twitter.api.call(paste0( url, 
            "&cursor=", cursor))
        results.so.far <- union(results.so.far, the.call[[field.to.extract]])
    }
    return(results.so.far)
}


get.screen.names <- function(userids) {
    lookup.call <- make.manual.twitter.api.call(
        paste0("https://api.twitter.com/1.1/users/lookup.json?user_id=",
            paste(userids,collapse=",")))
    return(lookup.call$screen_name)
}


main <- function() {
    U <<- collect.user.set(c("hzontine","rockladyeagles"))
}
