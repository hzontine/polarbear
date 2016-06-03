
FOLLOWERS_CACHE_NAME <- "followersCache.csv"
FOLLOWEES_CACHE_NAME <- "followeesCache.csv"

read.caches <- function() {
    if (file.exists(FOLLOWERS_CACHE_NAME)) {
        followers.cache <<- read.csv(FOLLOWERS_CACHE_NAME)
    } else {
        followers.cache <<- data.frame(userid=0,follower=0)
    }
    colnames(followers.cache) <- c("userid","follower")

    if (file.exists(FOLLOWEES_CACHE_NAME)) {
        followees.cache <<- read.csv(FOLLOWEES_CACHE_NAME)
    } else {
        followees.cache <<- data.frame(userid=0,followee=0)
    }
    colnames(followees.cache) <- c("userid","followee")
}

exists.in.cache <- function(userid, cache) {
    return(userid %in% cache[,1])
}

get.cached.values <- function(userid, cache) {
    return(cache[cache$userid == userid,2])
}

add.to.cache <- function(userid, values, cache.name) {
    if (exists.in.cache(userid, get(cache.name))) {
        warning(paste0(userid, " already in ", cache.name, "!"))
        return(NULL)
    }
    new.rows <- cbind(userid, values)
    colnames(new.rows) <- colnames(get(cache.name))
    assign(cache.name, rbind(get(cache.name), new.rows), .GlobalEnv)
}

flush.caches <- function() {
    write.csv(followers.cache, FOLLOWERS_CACHE_NAME, row.names=FALSE)
    write.csv(followees.cache, FOLLOWEES_CACHE_NAME, row.names=FALSE)
}


test.cache <- function() {
    read.caches()
    add.to.cache(5,c(10,15,20), "followers.cache")
    add.to.cache(11,c(20,25,30), "followers.cache")
    add.to.cache(17,c(1), "followers.cache")
    flush.caches()
    if (exists.in.cache(14, followers.cache)) {
        stop("14 shouldn't be cached!")
    }
    if (!exists.in.cache(11, followers.cache)) {
        stop("11 should be cached!")
    }
    cat("Here are 11's followers: ", 
        get.cached.values(11, followers.cache), "\n")
    add.to.cache(12,c(30,35), "followers.cache")
    if (!exists.in.cache(12, followers.cache)) {
        stop("12 should be cached!")
    }
    cat("Here are 12's followers: ", 
        get.cached.values(12, followers.cache), "\n")
}
