# Load packages
library(tidyverse)
library(text2vec)
library(LiblineaR)

# Set working directory
file.path("~", "Desktop", "App") %>% setwd

load("labeled_tweets.rda")

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

# Create word vector matrix using GloVe
nu_it <- itoken(labeled_tweets$text, preprocessor = preprocessor, tokenizer = word_tokenizer, progressbar = F)
nu_vocab <- create_vocabulary(nu_it, ngram = c(ngram_min = 1, ngram_max = 3), sep_ngram = "_")
nu_vocab <- prune_vocabulary(nu_vocab, term_count_min = 7, doc_proportion_min = .3)
nu_vectorizer <- vocab_vectorizer(nu_vocab)
tcm <- create_tcm(nu_it, nu_vectorizer, skip_grams_window = 5L)
glove_model <- GlobalVectors$new(word_vectors_size = 300, vocabulary = nu_vocab, x_max = 10)
wvm <- glove_model$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01) + t(glove_model$components)
rownames(wvm) <- nu_vocab$term

common_terms <- intersect(vocab$term, nu_vocab$term)
dtm <- dtm[, common_terms]
wvm <- wvm[]

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
