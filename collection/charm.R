
library(methods)

# Reminder: you have to create an "app" so that you can connect to the API.
# (You're not really creating an app, it's more of just a connection
# point.) If you still have your info from before, that's fine, you don't need
# to create another app.
#
# To create an app and get the info:
#
# Go to http://dev.twitter.com, and make an account if you don't have one.
#
#   Click on "Manage your Apps" at the very bottom.
#
#   Create a New App. Important: the "Callback URL" must be this
#   (copy/paste it): http://127.0.0.1:1410
#
#   When the app is created, you should be able to go to a "Keys and
#   Access Tokens" tab. The two pieces of info you need are:
#
#   1. the "key" (also called "consumer key")
#   2. the "secret" (also called "consumer secret")
#
# Paste those below.

Charm <- setRefClass("Charm",
    fields = list(
        key="character",
        secret="character",
        access.token="character",
        num.refreshes="numeric"
    ),
    methods = list(
        initialize=function(...) {
            .self$initFields(...)
            num.refreshes <<- 0
        }
    )
)

Charm$methods(refresh=function() {
    auth <- paste("Basic",RCurl::base64(paste(key,secret,sep=":")))
    auth.req <- POST("https://api.twitter.com/oauth2/token",
        add_headers(
            "Authorization"=auth,
            "Content-Type"="application/x-www-form-urlencoded;charset=UTF-8"),
        body="grant_type=client_credentials")
    access.token <<- paste("Bearer",content(auth.req)$access_token)
    num.refreshes <<- num.refreshes + 1
})


hannah.key <- "mGwXg8u650fqEeTAM7T1jDqMX"
hannah.secret <- "WOavl2fMDCL0QxFkEoYswy6FWTBDRLvaN8DtpUbbpKRCOtSDE1"
hannah <- Charm$new(key=hannah.key, secret=hannah.secret)

liv.key <- "kzy13nReYwPakG1jflt2zPVUm"
liv.secret <- "7t5E77ZzURWMJkFHS1J9SdIDThWounX9DMiWXRpN61je86vjN8"
liv <- Charm$new(key=liv.key, secret=liv.secret)

aaron.key <- "QqNt2fPekjeu9jNzuyfIwxC2q"
aaron.secret <- "pidb0hZR2WtJwo594KnKEc0w9KcYOzwtubjLDKdecbtxOXRsb1"
aaron <- Charm$new(key=aaron.key, secret=aaron.secret)

stephen.key <- "MlUmay5kA1vGWKokmmFofgRLX"
stephen.secret <- "2FFYCyI2rhUIEltkepeNhVcZvYufXJukCJMqE1s3ALKoLYm7LD"
stephen <- Charm$new(key=stephen.key, secret=stephen.secret)

dave.key <- "j9ylpEcXdHxZLZnN5OzH9CbNE"
dave.secret <- "VeYV8XXHbUCLk43iOWVOYB6ODzgwODwj0AFJOXANUGm36DKJYI"
dave <- Charm$new(key=dave.key, secret=dave.secret)


# An authentication "charm" is Stephen's name for a thing that will give you
# permissions to talk to Twitter. It contains an unchanging key and secret
# (from an app someone made on dev.twitter.com), plus a "refreshed"
# authentication token that should be recent.
charm.repo <- list(hannah, liv, aaron, stephen, dave)

current.charm.num <- 0

get.charm <- function() {
    current.charm.num <<- current.charm.num %% length(charm.repo) + 1
    charm.repo[[current.charm.num]]$refresh()
    return(charm.repo[[current.charm.num]])
}
