library(shiny)
library(leaflet)

load("choro.rda")

ui <- fluidPage(
  titlePanel("Interactive Choropleth of Manhattan/Bronx/Queens Neighborhoods by Percentage of Private Tweets"),
  leafletOutput("choro")
)

server <- function(input, output, session) {
  output$choro <- renderLeaflet({ choro })
}

shinyApp(ui, server)