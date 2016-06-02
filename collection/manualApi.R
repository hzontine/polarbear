library(RCurl)
library(httr)
library(jsonlite)

source("charm.R")

auth <- paste("Basic",RCurl::base64(paste(key,secret,sep=":")))
auth.req <- POST("https://api.twitter.com/oauth2/token",
    add_headers(
        "Authorization"=auth,
        "Content-Type"="application/x-www-form-urlencoded;charset=UTF-8"),
    body="grant_type=client_credentials")

access.token <- paste("Bearer",content(auth.req)$access_token)

DEBUG <- TRUE
make.manual.twitter.api.call <- function(the.api.request) {
    if (DEBUG) {
        cat("make.manual.twitter.api.call(\"",the.api.request,"\")...\n",
            sep="")
    }
    payload <- fromJSON(content(
        GET(the.api.request,add_headers(Authorization=access.token)),
        as="text"))
    if ("errors" %in% names(payload)) {
        cat("API barfed: ", payload$errors$message,"\n",sep="")
        return(NULL)
    }
    return(payload)
}

#For example:
#hannahs.info <- make.manual.twitter.api.call("https://api.twitter.com/1.1/users/lookup.json?screen_name=hzontine")
#cat(hannahs.info$description,"\n")
