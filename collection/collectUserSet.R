
library(doParallel)
registerDoParallel(8)

source("manualApi.R")
source("charm.R")
initialize.charms()


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
# include.screennames -- if TRUE, attach a "screenname" attribute to each
# vertex giving their (current) Twitter screenname.
#
# verbose -- if TRUE, print breadcrumb trail to the screen.
#
collect.user.set <- function(S, only.bidirectional=FALSE, 
    include.screennames=TRUE, verbose=TRUE) {

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

    charm <- get.charm()
    for (s in S) {
        if (verbose) cat("Processing seed node ",s,"...\n", sep="")
        this.seed.nodes.followers <- get.followers.of.screenname(s, charm)
        followers.of.S <- union(followers.of.S, this.seed.nodes.followers)
    }
    return.charm(charm)

    if (verbose) cat("There are ", length(followers.of.S), 
        " followers of the seed set.\n", sep="")

    U.edgelist <- 
        foreach (i=1:length(followers.of.S), .combine=rbind) %do% {

        # Get a charm to use for this 
        charm <- get.charm()
        while (is.null(charm)) {
            if (verbose) cat("No charms. Wait for one...\n")
            Sys.sleep(sample(60:120,1))
            charm <- get.charm()
        }

        u <- followers.of.S[i]

        if (verbose) cat("Deciding whether to retain ", u, "...\n", sep="")

        u.followers <- get.followers.of.userid(u, charm)
        u.followees <- get.followees.of.userid(u, charm)
        if ((!only.bidirectional && 
                any(u.followers %in% followers.of.S))  ||
             (only.bidirectional && 
                any(u.followers %in% followers.of.S) && 
                any(u.followees %in% followers.of.S))) {

            if (verbose) cat("  Retaining ", u, ".\n", sep="")
            
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
            return.charm(charm)
            return(rbind(u.follower.rows, u.followee.rows))

        } else {
            if (verbose) cat("  Dropping ", u, ".\n", sep="")
            return.charm(charm)
            return(NULL)
        }
    }


    # *barfs*
    U.edgelist <- unique(matrix(as.character(unlist(U.edgelist)),ncol=2))
    
    U <- graph_from_edgelist(U.edgelist)
    vertices.to.retain <- 
        V(U)[degree(U, mode="in") > 0  &  degree(U, mode="out") > 0]

    charm <- get.charm()
    if (include.screennames) {
        V(U)$screenname <- get.screennames(V(U)$name, charm)
    }
    return.charm(charm)
    return(induced_subgraph(U, vertices.to.retain))
}

get.followers.of.screenname <- function(screenname, charm, verbose=TRUE) {
    if (verbose) cat("Getting ", screenname, "'s followers...\n", sep="")
    if (simulated) return(get.simulated.userids.not.including(0))
    return(perform.cursor.call(paste0(
        "https://api.twitter.com/1.1/followers/ids.json?screen_name=",
            screenname), "ids", charm))
}

get.followers.of.userid <- function(userid, charm) {
    if (simulated) return(get.simulated.userids.not.including(userid))
    return(perform.cursor.call(paste0(
        "https://api.twitter.com/1.1/followers/ids.json?user_id=",
            userid), "ids", charm))
}

get.followees.of.userid <- function(userid, charm) {
    if (simulated) return(get.simulated.userids.not.including(userid))
    return(perform.cursor.call(paste0(
        "https://api.twitter.com/1.1/friends/ids.json?user_id=",
            userid), "ids", charm))
}

perform.cursor.call <- function(url, field.to.extract, charm) {
    results.so.far <- vector()
    call.num <- 1
    cat("Making call #", call.num, "...\n", sep="")
    the.call <- make.manual.twitter.api.call(paste0(
        url, "&cursor=-1"), charm)
    results.so.far <- union(results.so.far, the.call[[field.to.extract]])
    cursor <- the.call$next_cursor
    while (!is.null(cursor) && cursor != 0) {
        call.num <- call.num + 1
        cat("Making call #", call.num, "...\n", sep="")
        the.call <- make.manual.twitter.api.call(paste0( url, 
            "&cursor=", cursor), charm)
        results.so.far <- union(results.so.far, the.call[[field.to.extract]])
        cursor <- the.call$next_cursor
    }
    return(results.so.far)
}


get.screennames <- function(userids, verbose=TRUE, charm) {
    if (simulated) return (get.simulated.screennames(length(userids)))
    if (verbose) cat("Getting ",length(userids), " screennames...\n", sep="")
    screennames <- vector(length=length(userids))
    for (chunk.num in 1:ceiling(length(userids)/100)) {
        if (verbose) cat("chunk number ",chunk.num, "...\n", sep="")
        chunk.range <- (1+100*(chunk.num-1)):min(100*chunk.num,length(userids))
        lookup.call <- make.manual.twitter.api.call(
            paste0("https://api.twitter.com/1.1/users/lookup.json?user_id=",
                paste(userids[chunk.range],collapse=",")), charm)
        screennames[chunk.range] <- lookup.call$screen_name
    }
    return(screennames)
}



#############################################################################
# Just so I can test this thing without hitting the API every time, if
# "simulated" is TRUE generate fake API call responses.

simulated <- TRUE

get.simulated.userids.not.including <- function(userid) {
    users <- sample(1:200,5)
    return(users[users != userid])
}

get.simulated.screennames <- function(n) {
    ret.val <- vector(length=n)
    for (i in 1:n) {
        ret.val[i] <- paste0("@",paste0(sample(letters,3),collapse=""))
    }
    return(ret.val)
}

#############################################################################

local.peeps <- c(
    "rockladyeagles",
    "hzontine",
    "pinkcamowheelie",
    "raechick",
    "ZachWhitt2",
    "persnickery"
)

r.people <- c("hadleywickham","GaborCsardi","robjhyndman")

main <- function() {
    U <<- collect.user.set(local.peeps, only.bidirectional=FALSE)
    plot(U, vertex.label=V(U)$screenname, vertex.size=21)
}
