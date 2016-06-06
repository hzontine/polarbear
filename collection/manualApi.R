library(RCurl)
library(httr)
library(jsonlite)
library(stringr)

source("charm.R")

if (!exists("charm.repo")) {
    initialize.charms()
}
# active.charm is now in the global environment.


# For "very verbose."
UBER <- 2

make.manual.twitter.api.call <- function(the.api.request, verbose=FALSE) {

    parsed.str <- str_match(the.api.request, "1\\.1/(.*)/(.*)\\.json")
    resource.name <- parsed.str[[2]]
    op.name <- parsed.str[[3]]

    if (verbose==UBER) {
        cat("make.manual.twitter.api.call(\"",the.api.request,
            "\")...\n", sep="")
        cat("  (this is a ", resource.name, " resource request.)\n", sep="")
    }

    payload <- fromJSON(content(GET(the.api.request,
            add_headers(Authorization=active.charm$access.token)),
        as="text"))

    if ("errors" %in% names(payload)) {
        if (payload$errors$code == 88) {

            rate.limit.payload <- fromJSON(content(
                GET(paste0("https://api.twitter.com/1.1/application/",
                    "rate_limit_status.json?resources=", resource.name),
                    add_headers(Authorization=active.charm$access.token)),
                as="text"))
            limit <- rate.limit.payload$resources[[resource.name]][[
                paste("",resource.name,op.name,sep="/")]]$limit
            remaining <- rate.limit.payload$resources[[resource.name]][[
                paste("",resource.name,op.name,sep="/")]]$remaining
            if (verbose==UBER) {
                cat("Rate limit hit! ", remaining, "/", limit, 
                    " remaining.\n", sep="")
            }
            old.charm <- active.charm
            new.charm <- get.charm()
            if (verbose) cat("Swapping ", old.charm$name, " charm for ", 
                new.charm$name, ".\n\n", sep="")
            active.charm <<- new.charm

            return(make.manual.twitter.api.call(the.api.request))
        }
        stop("API barfed: ", payload$errors$message,"\n",sep="")
    }
    return(payload)
}

how.long.before.i.can <- function(the.api.request, verbose=FALSE) {

    parsed.str <- str_match(the.api.request, "1\\.1/(.*)/(.*)\\.json")
    resource.name <- parsed.str[[2]]
    op.name <- parsed.str[[3]]

    payload <- fromJSON(content(GET(the.api.request,
            add_headers(Authorization=active.charm$access.token)),
        as="text"))

    if ("errors" %in% names(payload)) {
        if (payload$errors$code == 88) {

            rate.limit.payload <- fromJSON(content(
                GET(paste0("https://api.twitter.com/1.1/application/",
                    "rate_limit_status.json?resources=", resource.name),
                    add_headers(Authorization=active.charm$access.token)),
                as="text"))
            reset <- rate.limit.payload$resources[[resource.name]][[
                paste("",resource.name,op.name,sep="/")]]$reset
            dt <- as.POSIXct(reset,origin="1970-01-01 00:00:00 EST") - 
                Sys.time()
            units(dt) <- "mins"
            cat("You must wait",round(dt,2),"minutes to do that.\n")
        } else {
            stop("API barfed: ", payload$errors$message,"\n",sep="")
        }
        return(invisible(NULL))
    }
    cat("Go for it!\n")
}

how.long <- function() {
    how.long.before.i.can(
        "https://api.twitter.com/1.1/followers/ids.json?screen_name=Batman")
}
#For example:
#hannahs.info <- make.manual.twitter.api.call("https://api.twitter.com/1.1/users/lookup.json?screen_name=hzontine")
#cat(hannahs.info$description,"\n")
