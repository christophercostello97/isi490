library(shiny)
library(text2vec)
library(SparseM)
library(LiblineaR)

load("pa.rda")

ui <- fluidPage(
  titlePanel("Hello Shiny!"),
  sidebarLayout(
    sidebarPanel(
      p("Sure, the model isn't perfect! But this application demonstrates real-time clasification using R Shiny!"),
      textInput(
        "text",
        label = NULL,
        value = "",
        width = NULL,
        placeholder = "Compose Tweet here"
      )
    ),
    mainPanel(
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  output$table <- renderTable({
    preprocessor <- function(txt) textstem::lemmatize_strings(tolower(txt))
    nrmlz <- function(x) (x - min(x)) / (max(x) - min(x))
    it <- itoken(input$text, preprocessor = preprocessor, tokenizer = word_tokenizer, progressbar = F)
    vocab <- create_vocabulary(it, ngram = c(ngram_min = 1, ngram_max = 5), sep_ngram = "_")
    dtm <- fit_transform(create_dtm(it, pa$vectorizer), TfIdf$new())
    dtm <- as.matrix.csr(new("matrix.csc", ra = dtm@x, ja = dtm@i + 1L, ia = dtm@p + 1L, dimension = dtm@Dim))
    pred <- predict(pa$model, dtm, decisionValues = T)
    certainty <- tail(nrmlz(append(pa$certainty, pred$decisionValues[, 1])), 1)
    data.frame(
      Tweet = input$text,
      Privacy = pred$predictions,
      Certainty = certainty
    )
  })
}

shinyApp(ui, server)
