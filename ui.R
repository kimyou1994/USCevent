library(datasets)
library(shiny)
library(shinydashboard)
library(plotly)

dashboardPage(
  dashboardHeader(title = "A Trojan Hacker's Day"), 
    #This is the search bar for the keyword inputs
    dashboardSidebar(
      textInput("Keywords", "Keywords separated by space: ", "food CS fun robots music data", width = "100%")
    ),
  dashboardBody(
    #This display the bubble chart graph
    plotlyOutput("plot"),
    DT::dataTableOutput("table")
  )
)