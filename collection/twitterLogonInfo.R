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

# This is Hannah:
hannah <- list()
hannah$key <- "mGwXg8u650fqEeTAM7T1jDqMX"
hannah$secret <- "WOavl2fMDCL0QxFkEoYswy6FWTBDRLvaN8DtpUbbpKRCOtSDE1"
hannah$access_token <- "394926716-dHC5EZtfYQI0fgno0Yitvx4doHuz8JicBNxFBg6z"
hannah$access_token_secret <- "KncwptmZl0KUfWbebINwOFvc4bbR7q2O2ixs7F19DPrXY"

# This is Liv:
liv <- list()
liv$key <- "kzy13nReYwPakG1jflt2zPVUm"
liv$secret <- "7t5E77ZzURWMJkFHS1J9SdIDThWounX9DMiWXRpN61je86vjN8"
liv$access_token <- "3648031337-eefjrJKBbe7xqqpdXcQF9DLpxvpZYurRIJyiN9e"
liv$access_token_secret <- "D1doHvZKXPn0sqBGXiJNuNcpuJmN2Z7BzkFbbbZDGt8qH"

#Aaron
aaron <- list()
aaron$key <- "QqNt2fPekjeu9jNzuyfIwxC2q"
aaron$secret <- "pidb0hZR2WtJwo594KnKEc0w9KcYOzwtubjLDKdecbtxOXRsb1"
aaron$access_token <- "551419186-TfWdgD0yNyipiWfVGx89rnwa8DOcUVGzQd1kd60d"
aaron$access_token_secret <- "TTiEJxJRvix00VuI3yDgFcmJzxjEqjhayGuPINED4IYGU"

# This is Stephen:
stephen <- list()
stephen$key <- "MlUmay5kA1vGWKokmmFofgRLX"
stephen$secret <- "2FFYCyI2rhUIEltkepeNhVcZvYufXJukCJMqE1s3ALKoLYm7LD"
stephen$access_token <- "1019144197-cMFsHfxTZiG0oyOidzM7bCW4uX6PzjXVOyNQTUK"
stephen$access_token_secret <- "fmW3KSw0kSYH43QPqSxUscnZD8XF8M8oEJnKm2sRAcq70"

# This is Dave:
dave <- list()
dave$key <- "j9ylpEcXdHxZLZnN5OzH9CbNE"
dave$secret <- "VeYV8XXHbUCLk43iOWVOYB6ODzgwODwj0AFJOXANUGm36DKJYI"
dave$access_token <- "703433868-5F8OLs2t2w8Xdkbr9LFbtreyNP4lo4faM4BDq8fY"
dave$access_token_secret <- "cwsYGxS64oR6np5XeqwdG0p2u3rky6xDEpcRm13TtMAZN"

auth.repo <- list(hannah, liv, aaron, stephen, dave)

