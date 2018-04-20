# Load packages
library(tidyverse)
library(text2vec)
library(reticulate)
library(LiblineaR)
gensim.models <- import("gensim.models.keyedvectors")

# Set working directory
file.path("~", "Desktop", "App") %>% setwd

# Load and downsample Tweets
tweets <- file.path("labeled_tweets.csv") %>%
  read_csv(col_types = cols(text = col_character(), privacy = col_factor(levels = NULL))) %>%
  group_by(privacy) %>% 
  add_count %>% 
  slice(sample(row_number(), min(.$n))) %>% 
  select(-n)

# Create DTM
preprocessor <- function(txt) txt %>% tolower %>% textstem::lemmatize_strings()
it <- itoken(tweets$text, preprocessor = preprocessor, tokenizer = word_tokenizer, progressbar = F)
vocab <- create_vocabulary(it, ngram = c(ngram_min = 1, ngram_max = 3), sep_ngram = "_")
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)

# Extract relevant Google News Word2Vec word vectors
w2v <- file.path("~", "GoogleNews-vectors-negative300.bin") %>%
  gensim.models$KeyedVectors$load_word2vec_format(., binary = T, limit = 500000L)
common_terms <- intersect(vocab$term, names(w2v$vocab))
dtm <- dtm[, common_terms]
wvm <- do.call(rbind, lapply(common_terms, w2v$word_vec))

# Create document vector matrix from averages of WVs by document
dvm <- t(apply(dtm, 1, function(dtm_row, wvm) {
  vs <- wvm[dtm_row > 0, ] * dtm_row[dtm_row > 0]
  if (is.matrix(vs)) { colMeans(vs) } else vs
}, wvm))

# Partition data into training and testing sets
train_index <- nrow(dvm) %>% sample(., (. * .8) %>% floor)
train <- list(x = dvm[train_index, ], y = tweets[train_index, ]$privacy)
test <- list(x = dvm[-train_index, ], y = tweets[-train_index, ]$privacy)

# Train linear SVM
model <- LiblineaR(data = train$x, target = train$y, type = 1)
pred <- predict(model, test$x)[[1]]

# Analyze results ("sensitivity" and "pos pred value" are recall and precision, respectively)
caret::confusionMatrix(pred, test$y, positive = "private")
