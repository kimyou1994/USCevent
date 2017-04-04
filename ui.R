library(datasets)
library(shiny)
library(plotly)

fluidPage(
  titlePanel("A Trojan Hacker's Day"), 
  fluidRow(
    #This is the search bar for the keyword inputs
    sidebarPanel(
      textInput("Keywords", "Keywords separated by space: ", "food CS fun robots music data", width = "60%"),
      h4("Input a sequence of keywords that shows your interest."),
      h4("Find the right event for today!")
    ),
    
    mainPanel(
      fluidRow(
        #This is image
        plotOutput("wordcloud")
      )
    )
  ),
  fluidRow(
    #This display the bubble chart graph
    plotlyOutput("plot"),
    DT::dataTableOutput("table")
  )
)