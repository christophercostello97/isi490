# Load packages
library(tidyverse)
library(magrittr)
library(text2vec)
library(LiblineaR)

# Set working directory
file.path("~", "Desktop", "App") %>% setwd

# Load and downsample Tweets
tweets <- file.path("labeled_tweets.csv") %>%
  read_csv(col_types = cols(text = col_character(), privacy = col_factor(levels = NULL))) %>% 
  group_by(privacy) %>% 
  add_count %>% 
  slice(sample(row_number(), min(.$n))) %>% 
  select(-n)

# Create DTM weighed by TF-IDF
preprocessor <- function(txt) txt %>% tolower %>% textstem::lemmatize_strings()
it <- itoken(tweets$text, preprocessor = preprocessor, tokenizer = word_tokenizer, progressbar = F)
vocab <- create_vocabulary(it, ngram = c(ngram_min = 1, ngram_max = 5), sep_ngram = "_")
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer) %>%
  fit_transform(TfIdf$new()) %>%
  (function(x) new("matrix.csc", ra = x@x, ja = x@i + 1L, ia = x@p + 1L, dimension = x@Dim)) %>%
  SparseM::as.matrix.csr(.)

# Partition data into training and testing sets
train_index <- nrow(dtm) %>% sample(., (. * .8) %>% floor)
train <- list(x = dtm[train_index, ], y = tweets[train_index, ]$privacy)
test <- list(x = dtm[-train_index, ], y = tweets[-train_index, ]$privacy)

# Train linear SVM
model <- LiblineaR(data = train$x, target = train$y, type = 1)
pred <- predict(model, test$x)[[1]]

# Analyze results ("sensitivity" and "pos pred value" are recall and precision, respectively)
caret::confusionMatrix(pred, test$y, positive = "private")
