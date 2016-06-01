
source("manualApi.R")


# Given a set of "seed" screen names S, return a data frame for a graph. This
# data frame will contain all users in a set U who satisfy the following
# criteria: (1) each user follows at least one element of S, and (2) each
# element follows at least one other element of U. (See only.bidirectional
# description for a tighter criterion.)
#
# The data frame will be in the form of an edge list, with column 1 being the
# follower and column 2 the followee.
#
# only.bidirectional -- if TRUE, then the only users who will be returned are
# those who *both* follow at least one element of U *and* are followed by at
# least one element (possibly different) of U.
#
# verbose -- print breadcrumb trail to the screen.
#
collect.user.set <- function(S, only.bidirectional=FALSE, verbose=TRUE) {

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

    U.edgelist <- matrix(nrow=0,ncol=2)
    for (u in followers.of.S) {

        if (verbose) cat("Checking ", u, "...\n", sep="")

        u.followers <- get.followers.of.userid(u)
        u.followees <- vector()
        if (only.bidirectional) {
            u.followees <- get.followees.of.userid(u)
        }
        if ((!only.bidirectional && 
                any(u.followers %in% followers.of.S))  ||
             (only.bidirectional && 
                any(u.followers %in% followers.of.S) && 
                any(u.followees %in% followers.of.S))) {

            if (verbose) cat("Including ", u, ".\n", sep="")
            
            if (length(u.followers) > 0) {
                u.follower.rows <- cbind(u,u.followers)
            } else {
                u.follower.rows <- matrix(nrow=0,ncol=2)
            }
            if (length(u.followees) > 0) {
                u.followee.rows <- cbind(u.followees,u)
            } else {
                u.followee.rows <- matrix(nrow=0,ncol=2)
            }
            U.edgelist <- rbind(U.edgelist, u.follower.rows, u.followee.rows)

        } else {
            # if (verbose) cat("Excluding ", u, ".\n", sep="")
        }
    }

    # *barfs*
    U.edgelist <- matrix(as.character(unlist(U.edgelist)),ncol=2)
    
    U <- graph_from_edgelist(U.edgelist)
    vertices.to.retain <- 
        V(U)[degree(U, mode="in") > 0  &  degree(U, mode="out") > 0]
    return(induced_subgraph(U, vertices.to.retain))
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

get.followees.of.userid <- function(userid) {
    return(perform.cursor.call(paste0(
        "https://api.twitter.com/1.1/friends/ids.json?user_id=",
            userid),"ids"))
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
    U <<- collect.user.set(c("rockladyeagles",
        "hzontine",
        "pinkcamowheelie",
        "raechick",
        "ZachWhitt2",
        "persnickery"
    ))
}
