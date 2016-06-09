
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

Charm <- setRefClass("Charm",
    fields = list(
        name="character",
        key="character",
        secret="character",
        access.token="character",
        charm.number="numeric",    # stupid, but don't know another way
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


hannah.name <- "Hannah"
hannah.key <- "mGwXg8u650fqEeTAM7T1jDqMX"
hannah.secret <- "WOavl2fMDCL0QxFkEoYswy6FWTBDRLvaN8DtpUbbpKRCOtSDE1"
hannah <- Charm$new(name=hannah.name, key=hannah.key, secret=hannah.secret)

hannah2.name <- "Hannah2"
hannah2.key="rpUEULlXgW1MTXeOS2YrDN0Dy"
hannah2.secret="DfJ57wcGNODc6nyld4cwVrxJiGnoSfphjMiYhRbpyJpww8YLgd"
hannah2 <- Charm$new(name=hannah2.name, key=hannah2.key, secret=hannah2.secret)

hannah3.name <- "Hannah3"
hannah3.key="DJH6DeGeodvdZOfwuP1eaRzIk"
hannah3.secret="FvcRfiVwD59nNgb3kmCTYPtn2KWiLPPi4TTHAx5x1VJ2EGA0jg"
hannah3 <- Charm$new(name=hannah3.name, key=hannah3.key, secret=hannah3.secret)

hannah4.name <- "Hannah4"
hannah4.key="JGWROlhDVcuagLp765FSkw0Rd"
hannah4.secret="ga4G8vLP5CDhgHkFD3ciT4a7RyPghiSk2xMrmNRzWboxsmlFM4"
hannah4 <- Charm$new(name=hannah4.name, key=hannah4.key, secret=hannah4.secret)

hannah5.name <- "Hannah5"
hannah5.key="FM0jwjo2WFqRnreiqhDsAmPGm"
hannah5.secret="AVEH9syhse6lGnAojiZXIHeivqJCJ7P1e6PGCmedwcnhh9NDct"
hannah5 <- Charm$new(name=hannah5.name, key=hannah5.key, secret=hannah5.secret)

hannah6.name <- "Hannah6"
hannah6.key="eoyL8ai68849R8EQ7noGA3xQW"
hannah6.secret="ycHxUVTOkbGowfeqKkTs0ANdIpOteF5ic7ELGZlGTvv3TGCipx"
hannah6 <- Charm$new(name=hannah6.name, key=hannah6.key, secret=hannah6.secret)

hannah7.name <- "Hannah7"
hannah7.key="2lgLl8hc3sj95YLncDFjE7yQe"
hannah7.secret="DL3uLEdaheqkq7862mTPnnlh6ry0rlsesTjfP2SgnwCeKd5xeL"
hannah7 <- Charm$new(name=hannah7.name, key=hannah7.key, secret=hannah7.secret)

hannah8.name <- "Hannah8"
hannah8.key="hgTBSs9tfhmtJaVk6xvrUryRl"
hannah8.secret="iVhrfPTOendP2rbQoPFzzYR9T99WXVj8XLwkFccgr1PjVhmnHT"
hannah8 <- Charm$new(name=hannah8.name, key=hannah8.key, secret=hannah8.secret)

hannah9.name <- "Hannah9"
hannah9.key="ToszGLdFAGqAxs8iHo9pvlWkE"
hannah9.secret="B9kfsaafq2akScWmgWt9mE56UNsJODyV4mGcRwPRJirpFM6jEE"
hannah9 <- Charm$new(name=hannah9.name, key=hannah9.key, secret=hannah9.secret)

hannah10.name <- "Hannah10"
hannah10.key="PYQGQrOfSbNxzyLAhjSIP4P1H"
hannah10.secret="iUryLbHvyloCo4s0EHgszpOAZ3BzIj0vbQv8SR2AfZLKG80kPR"
hannah10 <- Charm$new(name=hannah10.name, key=hannah10.key, secret=hannah10.secret)

hannah11.name <- "Hannah11"
hannah11.key="QemY30K5KhZTeKmr0kqZiXOLX"
hannah11.secret="9N7iBhl6gDyUdey7PsncHpsO2QobXdu68rMZnRlRkTYtwswEdS"
hannah11 <- Charm$new(name=hannah11.name, key=hannah11.key, secret=hannah11.secret)

hannah12.name <- "Hannah12"
hannah12.key="ouT1l2VUnIWoG1pUfe1WBQSMu"
hannah12.secret="iiJaZPzcK382QqHhDmlLHdpQLY25vgYTPMehPzAtwoURwY8l7A"
hannah12 <- Charm$new(name=hannah12.name, key=hannah12.key, secret=hannah12.secret)

hannah13.name <- "Hannah13"
hannah13.key="7SmI8l5vuVh2DsRxC0j1UeGvC"
hannah13.secret="bQwQrJJgiQaTz5y6qHGihMQyXcbPMJVyNcMMnbSTFKnyJbiZ4u"
hannah13 <- Charm$new(name=hannah13.name, key=hannah13.key, secret=hannah13.secret)

hannah14.name <- "Hannah14"
hannah14.key="xiq61FE0avCBZRlZ6vEzYKikg"
hannah14.secret="G2gZoGX434tJw955p9QtfhA7X8qb1YAZpaBbPS1h9DTpBuBiDi"
hannah14 <- Charm$new(name=hannah14.name, key=hannah14.key, secret=hannah14.secret)

hannah15.name <- "Hannah15"
hannah15.key="GlL27kSOcx6BtXfXr3ia5McEq"
hannah15.secret="5WLY8xebaRSaDjTUq6sKjhXMfqJJcK9p9RRUsOboYb5QxOHsvQ"
hannah15 <- Charm$new(name=hannah15.name, key=hannah15.key, secret=hannah15.secret)

hannah16.name <- "Hannah16"
hannah16.key="xuuAEFcO7ZQ97Eht8uSm3cHSh"
hannah16.secret="28QVtLt1S3pPgdFfdWOvYPSRKKhsb3SunzFTene5sPVNFfEwiT"
hannah16 <- Charm$new(name=hannah16.name, key=hannah16.key, secret=hannah16.secret)

liv.name <- "Liv"
liv.key <- "kzy13nReYwPakG1jflt2zPVUm"
liv.secret <- "7t5E77ZzURWMJkFHS1J9SdIDThWounX9DMiWXRpN61je86vjN8"
liv <- Charm$new(name=liv.name, key=liv.key, secret=liv.secret)

aaron.name <- "Aaron"
aaron.key <- "QqNt2fPekjeu9jNzuyfIwxC2q"
aaron.secret <- "pidb0hZR2WtJwo594KnKEc0w9KcYOzwtubjLDKdecbtxOXRsb1"
aaron <- Charm$new(name=aaron.name, key=aaron.key, secret=aaron.secret)

stephen.name <- "Stephen"
stephen.key <- "MlUmay5kA1vGWKokmmFofgRLX"
stephen.secret <- "2FFYCyI2rhUIEltkepeNhVcZvYufXJukCJMqE1s3ALKoLYm7LD"
stephen <- Charm$new(name=stephen.name, key=stephen.key, secret=stephen.secret)

dave.name <- "Dave"
dave.key <- "j9ylpEcXdHxZLZnN5OzH9CbNE"
dave.secret <- "VeYV8XXHbUCLk43iOWVOYB6ODzgwODwj0AFJOXANUGm36DKJYI"
dave <- Charm$new(name=dave.name, key=dave.key, secret=dave.secret)


# An authentication "charm" is Stephen's name for a thing that will give you
# permissions to talk to Twitter. It contains an unchanging key and secret
# (from an app someone made on dev.twitter.com), plus a "refreshed"
# authentication token that should be recent.

initialize.charms <- function() {
    charm.repo <<- list(hannah, hannah2, hannah3, hannah4, 
        hannah5, hannah6, hannah7, hannah8, hannah9, hannah10, 
        hannah11, hannah12, hannah13, hannah14, hannah15,
        hannah16, liv, aaron, stephen, dave)
    for (i in 1:length(charm.repo)) {
        charm.repo[[i]]$charm.number <- i
    }
    current.charm.num <<- 1
    active.charm <<- charm.repo[[1]]
    active.charm$refresh()
}


get.charm <- function(verbose=TRUE) {

    if (current.charm.num == length(charm.repo)) {
        # We've wrapped around and used all the charms. Impose a delay so we
        # don't wear Twitter out by continually trying to blow through the
        # rate limit stop sign.
        if (verbose) {
            cat("\nCharms exhausted. Chill before cycling through again.\n")
            print(Sys.time())
        }
        Sys.sleep(15*60+1)
        if (verbose) {
            print(Sys.time())
        }
    }
    current.charm.num <<- current.charm.num %% length(charm.repo) + 1

    next.charm <- charm.repo[[current.charm.num]]
    next.charm$refresh()
    return(next.charm)
}

