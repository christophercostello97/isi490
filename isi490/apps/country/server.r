library(shiny)
library(ggplot2)

load(file.path("Objects", "data.rda"))
df.world <- data[[1]]
df.terms <- data[[2]]

server <- function(input, output) {
  
  output$region.selector <- renderUI({ selectInput("region", "Select a region", as.list(unique(df.world$region)), "USA") })
  
  df.pie <- reactive({ df.world[df.world$region == input$region, ] })
  
  output$pie <- renderPlot({ 
    ggplot(df.pie(), aes(x = "", y = count, fill = legend)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
      theme(axis.text = element_blank(),
            axis.title.y = element_blank(),
            line = element_blank(),
            panel.background = element_blank()) +
      labs(y = paste0("English Tweets by topic and privacy class from\n", unique(df.pie()$region)), fill = "Legend")
  })
  
  output$terms <- renderTable({ df.terms })
  
}