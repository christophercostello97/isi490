library(shiny)

shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        textInput("user", "Insert user handle or ID", value = "realDonaldTrump", width = NULL, placeholder = NULL),
        actionButton("submit", "Pull Timeline", style = "margin-bottom: 15px;"),
        conditionalPanel(condition = "$('html').hasClass('shiny-busy')", shiny::tags$div("Loading...", style = "margin-bottom: 15px")),
        radioButtons("criteria", "Visualize by...", choices = c("Number", "Percent")),
        style = "margin-top: 15px;"
      ),
      mainPanel(
        plotOutput("breakdown"),
        plotOutput("by.day"),
        plotOutput("by.time")
      )
    )
  )
)