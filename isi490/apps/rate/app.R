# Packages
library(shiny)
library(RMySQL)

# User interface
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("batch",
        label = "Select a batch",
        choices = list(
          "Batch 1" = 1,
          "Batch 2" = 2,
          "Batch 3" = 3,
          "Batch 4" = 4,
          "Batch 5" = 5
        )
      ),
      radioButtons("tweet_1",
        label = "Tweet 1",
        choices = list("Not Private" = 0, "Private" = 1),
        inline = T
      ),
      radioButtons("tweet_2",
        label = "Tweet 2",
        choices = list("Not Private" = 0, "Private" = 1),
        inline = T
      ),
      radioButtons("tweet_3",
        label = "Tweet 3",
        choices = list("Not Private" = 0, "Private" = 1),
        inline = T
      ),
      radioButtons("tweet_4",
        label = "Tweet 4",
        choices = list("Not Private" = 0, "Private" = 1),
        inline = T
      ),
      radioButtons("tweet_5",
        label = "Tweet 5",
        choices = list("Not Private" = 0, "Private" = 1),
        inline = T
      ),
      actionButton("submit", "Submit ratings")
    ),
    mainPanel(
      tableOutput("sample")
    )
  )
)

# Database
load("queries.rda")

db <- dbConnect(
  drv = MySQL(),
  dbname = "isi490",
  host = "127.0.0.1",
  username = "root",
  password = "root"
)

onStop(function() dbDisconnect(db))

# Server
server <- function(input, output, session) {
  rater <- as.numeric(dbGetQuery(db, queries$max_rater)) + 1
  rater <- ifelse(is.na(rater), 1, rater)
  sample_tweets <- reactive({
    dbGetQuery(db, sprintf(queries$sample_tweets, input$batch, rater))
  })
  observeEvent(input$submit, {
    ids <- sample_tweets()$id
    insert_sql <- paste(
      sprintf("(%s, %s, %s),", ids[1], input$tweet_1, rater),
      sprintf("(%s, %s, %s),", ids[2], input$tweet_2, rater),
      sprintf("(%s, %s, %s),", ids[3], input$tweet_3, rater),
      sprintf("(%s, %s, %s),", ids[4], input$tweet_4, rater),
      sprintf("(%s, %s, %s)", ids[5], input$tweet_5, rater)
    )
    dbGetQuery(db, sprintf(queries$submit_ratings, insert_sql))
  })
  output$sample <- renderTable({
    data.frame(Tweets = sample_tweets()$text)
  })
}

# Configuration
shinyApp(ui, server)
