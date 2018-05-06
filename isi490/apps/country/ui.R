library(shiny)

ui <- fluidPage(
  titlePanel("English-language Tweets by topic and class per country"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("region.selector")
    ),
    mainPanel(
      plotOutput("pie"),
      tableOutput("terms")
    )
  )
)
