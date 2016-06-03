
# Two global variables exist: followers.cache and followers.cache.


read.caches <- function(force=FALSE) {

    if (exists("followers.cache") && !force) {
        cat("variable followers.cache already exists; NOT reading afresh!\n")
        return(NULL)
    }

    if (file.exists("followers.cache.csv")) {
        followers.cache <<- read.csv("followers.cache.csv", header=FALSE)
    } else {
        followers.cache <<- data.frame(userid=integer(),follower=integer())
    }
    colnames(followers.cache) <- c("userid","follower")

    if (file.exists("followees.cache.csv")) {
        followees.cache <<- read.csv("followees.cache.csv", header=FALSE)
    } else {
        followees.cache <<- data.frame(userid=integer(),followee=integer())
    }
    colnames(followees.cache) <- c("userid","followee")
}

exists.in.cache <- function(userid, cache) {
    return(userid %in% cache[,1])
}

get.cached.values <- function(userid, cache) {
    cached.values <- cache[cache[1] == userid,2]
    if (length(cached.values) == 1  &&  is.na(cached.values)) {
        return(NULL)
    } else {
        return(cached.values)
    }
}

add.to.cache <- function(userid, values, cache.name) {
    if (length(values) == 0) {
        # TODO: add value-less userids to the cache anyway so we don't re-ask
        # Twitter for them.
        return(NULL)
    }
    the.cache <- get(cache.name)
    if (exists.in.cache(userid, the.cache)) {
        warning(paste0(userid, " already in ", cache.name, "!"))
        return(NULL)
    }
    new.rows <- cbind(userid, values)
    colnames(new.rows) <- colnames(the.cache)
    assign(cache.name, rbind(the.cache, new.rows), .GlobalEnv)
    write.table(new.rows, paste0(cache.name,".csv"), row.names=FALSE, 
        col.names=FALSE, append=TRUE, sep=",")
}

flush.caches <- function() {
    write.csv(followers.cache, "followers.cache.csv", row.names=FALSE,
        col.names=FALSE)
    write.csv(followees.cache, "followees.cache.csv", row.names=FALSE,
        col.names=FALSE)
}


