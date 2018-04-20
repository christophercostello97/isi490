# Import packages
library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)

# Import project functions
source("twitter_api_functions.R", local = T)

# Import objects
load("twitter_token.rda")
load("pa.rda")

shinyServer(function(input, output) {
  tweets <- eventReactive(input$submit, {
    tweets.raw <- GetTimeline(input$user, n = 3200, token = twitter_token)
    preprocessor <- function(txt) textstem::lemmatize_strings(tolower(txt))
    nrmlz <- function(x) (x - min(x)) / (max(x) - min(x))
    it <- itoken(tweets.raw$text, preprocessor = preprocessor, tokenizer = word_tokenizer, progressbar = F)
    vocab <- create_vocabulary(it, ngram = c(ngram_min = 1, ngram_max = 5), sep_ngram = "_")
    dtm <- fit_transform(create_dtm(it, pa$vectorizer), TfIdf$new())
    dtm <- as.matrix.csr(new("matrix.csc", ra = dtm@x, ja = dtm@i + 1L, ia = dtm@p + 1L, dimension = dtm@Dim))
    class <- predict(pa$model, dtm, decisionValues = T)$predictions
    datetime.local <- GetDatetimeLocal(tweets.raw$created_at, tweets.raw$user.time_zone)
    day <- weekdays(datetime.local)
    time <- format(round_date(datetime.local, unit = "hour"), "%I:%M %p")
    tweets.proc <- data.frame(class, day, time)
    tweets.proc$day <- factor(tweets.proc$day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
    tweets.proc$time <- factor(tweets.proc$time, levels = rev(unique(OrderTime(time))))
    tweets.proc
  })
  output$breakdown <- renderPlot({
    tweets.breakdown <- tweets() %>% group_by(class) %>% tally()
    tweets.breakdown <- tweets.breakdown %>%
      mutate(per = round(n / sum(n) * 100, 2))
    ggplot(data = tweets.breakdown, mapping = aes(x = "", y = n, fill = class)) +
      coord_polar(theta = "y") +
      geom_bar(stat = "identity", width = 1) +
      geom_text(mapping = aes(label = paste0(per, "%")), position = position_stack(vjust = 0.5), color = "white", size = 3.5) +
      scale_fill_manual(values = c("#0000FF", "#FF0000")) +
      theme_minimal() +
      theme(legend.position = "bottom") + 
      labs(title = "Percentage of Tweets by class")
  })
  output$by.day <- renderPlot({
    tweets.by.day <- tweets() %>% group_by(class, day) %>% tally()
    tweets.by.day <- tweets.by.day %>% group_by(day) %>%
      mutate(per = round(n / sum(n) * 100, 2)) %>% ungroup
    if (input$criteria == "Number") {
      ggplot(data = tweets.by.day, mapping = aes(x = day, y = n, fill = class)) +
        geom_bar(stat = "identity") +
        geom_text(mapping = aes(label = paste0(per, "%")), position = position_stack(vjust = 0.5), color = "white", size = 3.5) +
        scale_fill_manual(values = c("#0000FF", "#FF0000")) +
        theme_minimal() +
        theme(legend.position = "bottom") + 
        labs(title = "Number of Tweets by day of week", x = "Day of Week", y = "Number of Tweets")
    } else {
      ggplot(data = tweets.by.day, mapping = aes(x = day, y = per, fill = class)) +
        geom_bar(stat = "identity") +
        geom_text(mapping = aes(label = paste0(per, "%")), position = position_stack(vjust = 0.5), color = "white", size = 3.5) +
        scale_fill_manual(values = c("#0000FF", "#FF0000")) +
        theme_minimal() +
        theme(legend.position = "bottom") + 
        labs(title = "Percentage of Tweets by day of week", x = "Day of Week", y = "Percentage of Tweets")
    }
  })
  output$by.time <- renderPlot({
    tweets.by.time <- tweets() %>% group_by(class, time) %>% tally()
    tweets.by.time <- tweets.by.time %>% group_by(time) %>%
      mutate(per = round(n / sum(n) * 100, 2)) %>% ungroup
    if (input$criteria == "Number") {
      ggplot(data = tweets.by.time, aes(x = time, y = n, fill = class)) +
        coord_flip() +
        geom_bar(stat = "identity") +
        geom_text(aes(label = paste0(per, "%")), position = position_stack(vjust = 0.5), color = "white", size = 3) +
        scale_fill_manual(values = c("#0000FF", "#FF0000")) + 
        theme_minimal() +
        theme(legend.position = "bottom") + 
        labs(title = "Number of Tweets by time of day", x = "Time of Day (rounded to nearest hour)", y = "Number of Tweets")
    } else {
      ggplot(data = tweets.by.time, aes(x = time, y = per, fill = class)) +
        coord_flip() +
        geom_bar(stat = "identity") +
        geom_text(aes(label = paste0(per, "%")), position = position_stack(vjust = 0.5), color = "white", size = 3) +
        scale_fill_manual(values = c("#0000FF", "#FF0000")) + 
        theme_minimal() +
        theme(legend.position = "bottom") + 
        labs(title = "Percentage of Tweets by time of day", x = "Time of Day (rounded to nearest hour)", y = "Percentage of Tweets")  
    }
  })
})