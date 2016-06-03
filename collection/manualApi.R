library(RCurl)
library(httr)
library(jsonlite)
library(stringr)

source("charm.R")


DEBUG <- TRUE
make.manual.twitter.api.call <- function(the.api.request, charm) {

    parsed.str <- str_match(the.api.request, "1\\.1/(.*)/(.*)\\.json")
    resource.name <- parsed.str[[2]]
    op.name <- parsed.str[[3]]

    if (DEBUG) {
        cat("make.manual.twitter.api.call(\"",the.api.request,"\")...\n",
            sep="")
        cat("  (this is a ", resource.name, " resource request.)\n", sep="")
    }

    payload <- fromJSON(content(
        GET(the.api.request,add_headers(Authorization=charm$access.token)),
        as="text"))

    if ("errors" %in% names(payload)) {
        if (payload$errors$code == 88) {

            rate.limit.payload <- fromJSON(content(
                GET(paste0("https://api.twitter.com/1.1/application/",
                    "rate_limit_status.json?resources=", resource.name),
                    add_headers(Authorization=charm$access.token)),
                as="text"))
            limit <- rate.limit.payload$resources[[resource.name]][[
                paste("",resource.name,op.name,sep="/")]]$limit
            remaining <- rate.limit.payload$resources[[resource.name]][[
                paste("",resource.name,op.name,sep="/")]]$remaining
            cat("Rate limit hit! ", remaining, "/", limit, " remaining.\n",
                sep="")
            cat("Sleeping for 15 minutes...\n")
            Sys.sleep(15*60)

            # We could try to be smart about how long to sleep (commented out
            # code, below) but this turns out to suck, since our clock isn't
            # synchronized with Twitter.
            #  time.for.reset <- rate.limit.payload$resources[[resource.name]][[
            #      paste("",resource.name,op.name,sep="/")]]$reset
            #  seconds.to.sleep <- (time.for.reset - 
            #      as.integer(as.POSIXct(Sys.time()))) / 1000
            #  cat("Rate limit hit! Sleeping for ", seconds.to.sleep, 
            #      "seconds...\n")
            #  Sys.sleep(seconds.to.sleep)
            cat("*yawn*\n")
            return(make.manual.twitter.api.call(the.api.request, charm))
        }
        stop("API barfed: ", payload$errors$message,"\n",sep="")
    }
    return(payload)
}

#For example:
#my.charm <- get.charm()
#hannahs.info <- make.manual.twitter.api.call("https://api.twitter.com/1.1/users/lookup.json?screen_name=hzontine",my.charm)
#cat(hannahs.info$description,"\n")
