
source("manualApi.R")
source("charm.R")
source("cache.R")


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
# least one element (possibly different) of U. If FALSE, the only thing that
# matters is the latter (i.e., only followers are checked).
#
# threshold.for.inclusion -- the minimum number of followees (and if
# only.bidirectional is TRUE, also the minimum number of followers) a vertex
# must have to be included in the final graph.
#
# include.screennames -- if TRUE, attach a "screenname" attribute to each
# vertex giving their (current) Twitter screenname.
#
# verbose -- if TRUE, print breadcrumb trail to the screen.
#
collect.user.set <- function(S, only.bidirectional=FALSE, 
    threshold.for.inclusion=1, include.screennames=TRUE, verbose=TRUE) {

    if (verbose) {
        if (length(S) < 10) {
            cat("Collecting user set for seed set:", S, "...\n")
        } else {
            cat("Collecting user set for seed set of size ",
                length(S), "...\n", sep="")
        }
    }

    read.caches()

    # A vector of the screen names of everyone who follows anyone in the seed
    # set.
    followers.of.S <- vector()

    for (s in S) {
        if (verbose) cat("Processing seed node ",s,"...\n", sep="")
        this.seed.nodes.followers <- get.followers.of.screenname(s)
        followers.of.S <- union(followers.of.S, this.seed.nodes.followers)
    }

    if (verbose) cat("There are ", length(followers.of.S), 
        " followers of the seed set.\n", sep="")

    U.edgelist <- matrix(nrow=0,ncol=2)

    for (i in 1:length(followers.of.S)) {

        u <- followers.of.S[i]

        if (verbose) cat("Deciding whether to retain ", u, "...\n", sep="")

        f <- dplyr::filter
        u.followers <- get.followers.of.userid(u, verbose)
        u.followees <- get.followees.of.userid(u, verbose)
        if ((!only.bidirectional && 
                nrow(f(u.followers,follower %in% followers.of.S)) >=
                        threshold.for.inclusion) ||
             (only.bidirectional && 
                nrow(f(u.followers,follower %in% followers.of.S)) >=
                        threshold.for.inclusion &&
                nrow(f(u.followees,followee %in% followers.of.S)) >=
                        threshold.for.inclusion)) {

            cat("  Retaining ", u, ".\n", sep="")
            
            u.followers <- as.data.frame(collect(select(u.followers,2)))[,1]
            if (length(u.followers) > 0) {
                u.follower.rows <- cbind(u,u.followers)
            } else {
                u.follower.rows <- matrix(nrow=0,ncol=2)
            }
            u.followees <- as.data.frame(collect(select(u.followees,2)))[,1]
            if (length(u.followees) > 0) {
                u.followee.rows <- cbind(u.followees,u)
            } else {
                u.followee.rows <- matrix(nrow=0,ncol=2)
            }
            U.edgelist <- rbind(U.edgelist, u.follower.rows, u.followee.rows)

        } else {
            if (verbose) cat("  Dropping ", u, ".\n", sep="")
        }
    }


    # *barfs*
    U.edgelist <- unique(matrix(as.character(unlist(U.edgelist)),ncol=2))
    
    U <- graph_from_edgelist(U.edgelist)
    if (only.bidirectional) {
        vertices.to.retain <- 
            V(U)[degree(U, mode="in") >= threshold.for.inclusion  &  
                 degree(U, mode="out") >= threshold.for.inclusion]
    } else {
        vertices.to.retain <- 
            V(U)[degree(U, mode="out") >= threshold.for.inclusion]
    }

    if (include.screennames) {
        V(U)$screenname <- get.screennames(V(U)$name)
    }

    return(induced_subgraph(U, vertices.to.retain))
}

get.followers.of.screenname <- function(screenname, verbose=TRUE) {
    if (verbose) cat("Getting ", screenname, "'s followers...\n", sep="")
    if (simulated) return(get.simulated.userids.not.including(0))
    return(perform.cursor.call(paste0(
        "https://api.twitter.com/1.1/followers/ids.json?screen_name=",
            screenname,"&stringify_ids=true"), "ids"))
}

get.followers.of.userid <- function(userid, verbose=FALSE) {
    if (exists.in.cache(userid, followers.cache)) {
        if (verbose) {
            cat("Returning cached followers results for user ", userid,
                "...\n", sep="")
        }
        return(get.cached.values(userid, followers.cache))
    }
    if (simulated) {
        followers <- get.simulated.userids.not.including(userid)
    } else {
        followers <- perform.cursor.call(paste0(
            "https://api.twitter.com/1.1/followers/ids.json?user_id=",
                userid,"&stringify_ids=true"), "ids")
    }
    add.to.cache(userid, followers, "followers.cache")
    return(get.followers.of.userid(userid, FALSE))
}

get.followees.of.userid <- function(userid, verbose=FALSE) {
    if (exists.in.cache(userid, followees.cache)) {
        if (verbose) {
            cat("Returning cached followees results for user ", userid,
                "...\n", sep="")
        }
        return(get.cached.values(userid, followees.cache))
    }
    if (simulated) {
        followees <- get.simulated.userids.not.including(userid)
    } else {
        followees <- perform.cursor.call(paste0(
            "https://api.twitter.com/1.1/friends/ids.json?user_id=",
                userid,"&stringify_ids=true"), "ids")
    }
    add.to.cache(userid, followees, "followees.cache")
    return(get.followees.of.userid(userid, FALSE))
}

perform.cursor.call <- function(url, field.to.extract) {
    results.so.far <- vector()
    call.num <- 1
    the.call <- make.manual.twitter.api.call(paste0(
        url, "&cursor=-1"))
    results.so.far <- union(results.so.far, the.call[[field.to.extract]])
    cursor <- the.call$next_cursor
    while (!is.null(cursor) && cursor != 0) {
        call.num <- call.num + 1
        cat("Making repeated call #", call.num, "...\n", sep="")
        the.call <- make.manual.twitter.api.call(paste0( url, 
            "&cursor=", cursor))
        results.so.far <- union(results.so.far, the.call[[field.to.extract]])
        cursor <- the.call$next_cursor
    }
    return(results.so.far)
}


get.screennames <- function(userids, verbose=TRUE) {
    if (simulated) return (get.simulated.screennames(length(userids)))
    if (verbose) cat("Getting ",length(userids), " screennames...\n", sep="")
    screennames <- rep(NA,length(userids))
    userid.indices.we.need.to.bug.twitter.for <- vector()
    for (i in 1:length(userids)) {
        if (exists.in.cache(userids[i], screennames.cache)) {
            screennames[i] <- 
                as.data.frame(collect(
                    get.cached.values(userids[i], screennames.cache)))[1,2]
        } else {
            userid.indices.we.need.to.bug.twitter.for <- 
                c(userid.indices.we.need.to.bug.twitter.for, i)
        }
    }
    userids.we.need.to.bug.twitter.for <- 
        userids[userid.indices.we.need.to.bug.twitter.for]
    num.chunks <- ceiling(length(userids.we.need.to.bug.twitter.for)/100)
    if (num.chunks == 0) {
        return(screennames)
    }
    screennames.we.bugged.twitter.for <- 
        vector(length=length(userids.we.need.to.bug.twitter.for))
    for (chunk.num in 
            1:ceiling(length(userids.we.need.to.bug.twitter.for)/100)) {
        if (verbose) {
            cat("Retrieving screenname chunk ",chunk.num, 
                " of ", num.chunks, "...\n", sep="")
        }
        chunk.range <- (1+100*(chunk.num-1)):min(100*chunk.num,
                                length(userids.we.need.to.bug.twitter.for))
        lookup.call <- make.manual.twitter.api.call(
            paste0("https://api.twitter.com/1.1/users/lookup.json?user_id=",
                paste(userids.we.need.to.bug.twitter.for[chunk.range],
                    collapse=",")))
        screennames.we.bugged.twitter.for[chunk.range] <- 
            lookup.call$screen_name
        add.to.cache(userids.we.need.to.bug.twitter.for,
            screennames.we.bugged.twitter.for, "screennames.cache")
    }
    screennames[userid.indices.we.need.to.bug.twitter.for] <-
        screennames.we.bugged.twitter.for
    return(screennames)
}



#############################################################################
# Just so I can test this thing without hitting the API every time, if
# "simulated" is TRUE generate fake API call responses.

simulated <- FALSE

get.simulated.userids.not.including <- function(userid) {
    users <- sample(1:200,sample(3:10,1))
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
    seed.set <- local.peeps
    U <<- collect.user.set(seed.set, only.bidirectional=TRUE,
        verbose=FALSE)
    plot(U, vertex.label=paste0("@",V(U)$screenname), vertex.size=6, 
        vertex.label.cex=.8, edge.arrow.size=.5, layout=layout_with_kk,
        vertex.color=ifelse(V(U)$screenname %in% seed.set,
            "dodgerblue","orange"))
}
