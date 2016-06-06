
library(dplyr)
library(DBI)

source("db.R")

# To set up MySQL schema:
# mysql> create table followers (userid varchar(20), follower varchar(20));
# mysql> create table followees (userid varchar(20), followee varchar(20));
# mysql> create table nodata_users (userid varchar(20));
# mysql> create table screennames (userid varchar(20), screenname varchar(16));


# TODO: if we already have a userid's followers cached, and we already have
# the screenname that goes with that userid, then we should be able to use the
# cache to find the followers for that screenname.

read.caches <- function(force=FALSE) {

    db.src <- src_mysql("polarbear",user="stephen",password="iloverae")
    followees.cache <<- tbl(db.src,"followees")
    followers.cache <<- tbl(db.src,"followers")
    screennames.cache <<- tbl(db.src,"screennames")
    nodata.cache <<- tbl(db.src,"nodata_users")
}

exists.in.cache <- function(the.userid, cache) {
    return(nrow(cache %>% dplyr::filter(userid==the.userid)) > 0 ||
           nrow(nodata.cache %>% dplyr::filter(userid==the.userid)) > 0)
}

get.cached.values <- function(the.userid, cache) {
    cached.values <- cache %>% dplyr::filter(userid == the.userid)
#    if (nrow(cached.values) == 1  &&  is.na(cached.values)) {
#        return(NULL)
#    } else {
        return(cached.values)
#    }
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
    if (exists.in.cache(userid, the.cache)) {
        warning(paste0(userid, " already in ", cache.name, "!"))
        return(NULL)
    }
    new.rows <- cbind(userid, values)
    colnames(new.rows) <- colnames(the.cache)

    mysql.table.name <- str_match(cache.name, "(.*)\\.cache")[[2]]
    conn <- get.connection(TRUE)
    dbGetQuery(conn,
        paste0("insert into ", mysql.table.name, " values (",
            paste("'",userid,"','",values,"'",sep="",collapse="),("),")"))
    dbDisconnect(conn)
}

