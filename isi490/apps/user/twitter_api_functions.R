# Import packages
library(jsonlite)
library(httr)
library(dplyr)
library(lubridate)

# Function to 
GetTwitterAPIToken <- function(consumer.key, consumer.secret) {
  secret <- jsonlite::base64_enc(paste(consumer.key, consumer.secret, sep = ":"))
  req <- httr::POST("https://api.twitter.com/oauth2/token",
                    httr::add_headers(
                      "Authorization" = paste("Basic", gsub("\n", "", secret)),
                      "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"
                    ),
                    body = "grant_type=client_credentials"
  );
  httr::stop_for_status(req, "authenticate with twitter")
  token <- paste("Bearer", httr::content(req)$access_token)
  return(token)
}

# Get Tweets from user timeline
GetTimeline <- function(username, n, max.id = NULL, token = twitter.token) {
  tweets <- NULL
  while (n > 0) {
    if (is.null(max.id)) {
      url <- paste0("https://api.twitter.com/1.1/statuses/user_timeline.json?count=", n, "&screen_name=", username)
    } else {
      url <- paste0("https://api.twitter.com/1.1/statuses/user_timeline.json?count=", n, "&max_id=", max.id, "&screen_name=", username)
    }
    req <- httr::GET(url, httr::add_headers(Authorization = token))
    json <- httr::content(req, as = "text")
    tweets.new <- jsonlite::fromJSON(json)
    tweets.new <- jsonlite::flatten(tweets.new)
    tweets <- bind_rows(tweets, tweets.new)
    n <- n - 200
    max.id <- as.character(min(tweets.new$id))
  }
  return(as.data.frame(tweets))
}

GetDatetimeLocal <- function(created_at, user.time_zone, datetime.input = F) {
  if (datetime.input == F) {
    created_at <- as.POSIXct(created_at, format = "%a %b %d %H:%M:%S %z %Y", tz = "UTC")
  }
  tz.index <- match(user.time_zone, tz$RoR)
  datetime.local.tz <- tz[tz.index, ]$IANA
  datetime.local <- with_tz(created_at, unique(datetime.local.tz))
}

OrderTime <- function(time) {
  time <- sort(time)
  if (any(grepl("12:", time))) {
    time.t <- time[grep("12:", time)]
    time.n <- time[-grep("12:", time)]
    time <- c(time.t, time.n)
  }
  if (any(grepl(" AM", time))) {
    time.am <- time[grep(" AM", time)]
    time.pm <- time[grep(" PM", time)]
    time <- c(time.am, time.pm)
  }
  return(time)
}
