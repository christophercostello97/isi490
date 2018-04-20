# Set Python
reticulate::use_python("/usr/local/bin/python3", required = T)

# Load packages
library(tidyverse)
library(magrittr)
library(reticulate)
library(cleanNLP)
library(text2vec)
library(LiblineaR)
html <- import("html")
cnlp_init_spacy()

# Tokenizer function
custom_tokenizer <- function(txt) {
  ann <- txt %>%
    gsub("https?:\\/\\/\\S+", "URL", .) %>%
    gsub("@\\w+", "MENTION", .) %>%
    gsub("#(?=\\w+)", "hashtag_", ., perl = T) %>%
    as.list %>% rapply(html$unescape) %>%
    #stringi::stri_trans_general("latin-ascii") %>%
    #iconv(from = "latin1", to = "ASCII", sub = "") %>%
    cnlp_annotate
  tkn <- cnlp_get_token(ann) %>%
    filter(upos != "PUNCT")
  ent <- cnlp_get_entity(ann)
  if (nrow(ent) > 0) {
    tkn %<>% left_join(ent, by = c("id", "sid", "tid"))
    s <- which(!is.na(tkn$entity_type))
    e <- s + str_count(tkn[s, ]$entity, "\\w+") - 1
    r <- unlist(Map(function(s, e) (s + 1):e, s, e))
    tkn %<>%
      slice(-r) %>%
      mutate(token = ifelse(is.na(entity_type), lemma, entity_type))
  }
  tkn %<>%
    mutate(id = id %>% as.factor %>% as.integer) %>%
    mutate(token = gsub("hashtag_(?=\\w+)", "#", token, perl = T)) %>%
    (function(tkn) {
      indx <- grepl("PRP", tkn$pos)
      tkn[indx, ]$token <- mapply(function(word, pos) {
        pronoun <- lexicon::pos_df_pronouns[grepl(paste0("^", word, "$"), lexicon::pos_df_pronouns$pronoun, ignore.case = T), ]
        pronoun <- if (pos == "PRP$") { pronoun[type == "possessive", ] } else pronoun[type != "possessive", ]
        ifelse(
          nrow(pronoun) == 1,
          paste(pronoun$point_of_view, pronoun$type, "PRN", sep = "_") %>%
            gsub("first", "1ST", .) %>% gsub("second", "2ND", .) %>% gsub("third", "3RD", .) %>%
            gsub("personal", "PRS", .) %>% gsub("reflexive", "RFX", .) %>% gsub("possessive", "PSV", .),
          word
        )
      }, tkn[indx, ]$word, tkn[indx, ]$pos)
      tkn
    }) %>%
    split(f = .$id) %>% unname %>% lapply(function(x) pull(x, token))
}

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
it <- itoken(tweets$text, tokenizer = custom_tokenizer, progressbar = F)
vocab <- create_vocabulary(it, ngram = c(ngram_min = 1, ngram_max = 3), sep_ngram = "_") %>%
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
