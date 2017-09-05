library(datasets)
library(shiny)
library(shinydashboard)
library(plotly)

dashboardPage(
  dashboardHeader(title = "A Trojan Hacker's Day"), 
    #This is the search bar for the keyword inputs
    dashboardSidebar(
      textInput("Keywords1", "keyword 1: ", "biology", width = "100%"),
      textInput("Keywords2", "keyword 2: ", "computer", width = "100%"),
      textInput("Keywords3", "keyword 3: ", "music", width = "100%"),
      textInput("Keywords4", "keyword 4: ", "happiness", width = "100%")
    ),
  dashboardBody(
    #This display the bubble chart graph
    plotlyOutput("plot"),
    DT::dataTableOutput("table")
  )
)