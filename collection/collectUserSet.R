
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

    # A vector of the screen names of everyone who follows anyone in the seed
    # set.
    followers.of.S <- vector()

    for (s in S) {
        if (verbose) cat("Processing ",s,"...\n", sep="")
        this.seed.nodes.followers <- get.followers.of.screenname(s)
        followers.of.S <- union(followers.of.S, this.seed.nodes.followers)
    }

    if (verbose) cat("There are ", length(followers.of.S), 
        " followers of the seed set.\n", sep="")

    U <- vector()
    for (u in followers.of.S) {
        if (verbose) cat("Checking ", u, "...\n", sep="")
        this.users.followers <- get.followers.of.userid(u)
        if (any(this.users.followers %in% followers.of.S)) {
            if (verbose) cat("Including ", u, ".\n", sep="")
            U <- union(U, u)
        } else {
            # if (verbose) cat("Excluding ", u, ".\n", sep="")
        }
    }

    return(get.screennames(U))
}


get.followers.of.screenname <- function(screenname, verbose=TRUE) {
    if (verbose) cat("Getting ", screenname, "'s followers...\n", sep="")
    return(perform.cursor.call(paste0(
        "https://api.twitter.com/1.1/followers/ids.json?screen_name=",
            screenname),"ids"))
}

get.followers.of.userid <- function(userid) {
    return(perform.cursor.call(paste0(
        "https://api.twitter.com/1.1/followers/ids.json?user_id=",
            userid),"ids"))
}

get.followees.of <- function(screenname) {
    return(perform.cursor.call(paste0(
        "https://api.twitter.com/1.1/friends/ids.json?screen_name=",
            screenname),"ids"))
}

perform.cursor.call <- function(url, field.to.extract) {
    results.so.far <- vector()
    the.call <- make.manual.twitter.api.call(paste0(
        url, "&cursor=-1"))
    if ("error" %in% names(the.call)) {
        cat(the.call[["error"]],"\n")
        return(NULL)
    }
    results.so.far <- union(results.so.far, the.call[[field.to.extract]])
    cursor <- the.call$next_cursor
    while (!is.null(cursor) && cursor != 0) {
        the.call <- make.manual.twitter.api.call(paste0( url, 
            "&cursor=", cursor))
        results.so.far <- union(results.so.far, the.call[[field.to.extract]])
        cursor <- the.call$next_cursor
    }
    return(results.so.far)
}


get.screennames <- function(userids, verbose=TRUE) {
    if (verbose) cat("Getting ",length(userids), " screennames...\n", sep="")
    screennames <- vector(length=length(userids))
    for (chunk.num in 1:ceiling(length(userids)/100)) {
        if (verbose) cat("chunk number ",chunk.num, "...\n", sep="")
        chunk.range <- (1+100*(chunk.num-1)):min(100*chunk.num,length(userids))
        lookup.call <- make.manual.twitter.api.call(
            paste0("https://api.twitter.com/1.1/users/lookup.json?user_id=",
                paste(userids[chunk.range],collapse=",")))
        screennames[chunk.range] <- lookup.call$screen_name
    }
    return(screennames)
}


main <- function() {
    U <<- collect.user.set(c("rockladyeagles","hzontine"))
}
