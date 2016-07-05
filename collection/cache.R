
library(dplyr)
library(DBI)

source("db.R")

# To set up MySQL schema:
# mysql> create table followers (userid varchar(20), follower varchar(20));
# mysql> create table followees (userid varchar(20), followee varchar(20));
# mysql> create table nodata_users (userid varchar(20));
# mysql> create table screennames (userid varchar(20), screenname varchar(16));
# mysql> create table tweets (userid varchar(20), tweet varchar(200));
# mysql> create table training (userid varchar(20), class char(1), timest
# timestamp);


# TODO: if we already have a userid's followers cached, and we already have
# the screenname that goes with that userid, then we should be able to use the
# cache to find the followers for that screenname.

read.caches <- function(force=FALSE) {

    if (!exists("mysql.db.name")) {
        source("mysql_config.R")
    }
    db.src <- src_mysql(mysql.db.name,user=mysql.user,password=mysql.password)
    followees.cache <<- tbl(db.src,"followees")
    followers.cache <<- tbl(db.src,"followers")
    screennames.cache <<- tbl(db.src,"screennames")
    nodata.cache <<- tbl(db.src,"nodata_users")
    tweets.cache <<- tbl(db.src,"tweets")
    training.cache <<- tbl(db.src,"training")
}

exists.in.cache <- function(the.userid, cache, check.nodata=TRUE) {
    in.cache <- nrow(cache %>% dplyr::filter(userid==the.userid)) > 0
    if (check.nodata) {
        return(in.cache || 
           nrow(nodata.cache %>% dplyr::filter(userid==the.userid)) > 0)
    } else {
        return(in.cache)
    }
}

get.cached.values <- function(the.userid, cache) {
    return(cache %>% dplyr::filter(userid == the.userid))
}

add.to.cache <- function(userid, values, cache.name) {
    if (length(values) == 0) {
        # Add value-less userids to the "nodata" table so we don't keep
        # asking Twitter for them.
        conn <- get.connection(TRUE)
        dbGetQuery(conn,
            paste0("insert into nodata_users values ('",userid,"')"))
        dbDisconnect(conn)
        return(NULL)
    }
    the.cache <- get(cache.name)
    new.rows <- cbind(userid, values)
    colnames(new.rows) <- colnames(the.cache)

    mysql.table.name <- str_match(cache.name, "(.*)\\.cache")[[2]]
    conn <- get.connection(TRUE)
    retry <- FALSE
    retries.left <- 1
    repeat { 
        tryCatch(
            dbGetQuery(conn,
                paste0("insert into ", mysql.table.name, " values (",
                    paste("'",userid,"','",dbEscapeStrings(conn,values),"'",
                            sep="",collapse="),("),")"))
            , error = function(e) {
                cat("The error message is:\n")
                print(e)
                retry <<- TRUE
            }
        )
        if (retry) {
            if (retries.left < 5) {
                cat("Retrying...getting new connection...\n")
                conn <<- get.connection(TRUE)
                retries.left <- retries.left + 1
            } else {
                cat("Giving up.\n")
                break
            }
        } else {
            break
        }
    }
    dbDisconnect(conn)
}

# Functions to support reverse lookup (e.g., userid for screenname).
value.exists.in.cache <- function(the.value, cache) {
    # I know, I know, it's a hack. First, using "screenname" explicitly here
    # instead of "whatever column 2 is" (which I can't figure out how to do in
    # dplyr) and second, storing a screenname in the nodata_users table, which
    # is supposedly made up of userids?!?
    return(nrow(cache %>% dplyr::filter(screenname == the.value)) > 0 ||
           nrow(nodata.cache %>% dplyr::filter(userid==the.value)) > 0)
}

get.cached.userid <- function(the.value, cache) {
    # (See "hack" comments, above.)
    return(cache %>% dplyr::filter(screenname == the.value))
}

