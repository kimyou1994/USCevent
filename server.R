library(datasets)
library(plotly)
library(shiny)
library(DT)
library(RXKCD)
library(RColorBrewer)
library(rvest)
library(stringr)
library(methods)
options(DT.fillContainer = FALSE)
options(DT.autoHideNavigation = FALSE)

main_url <- "https://calendar.usc.edu/calendar/day"
main_web <- read_html(main_url)
main_event <- main_web %>% html_nodes(".summary a") %>% html_text()
main_link <-
  as.character(main_web %>% html_nodes(".summary a")) # %>% html_attr("href")
main_loc <-
  main_web %>% html_nodes(".event_item_venue") %>% html_text()
main_event_content <-
  main_web %>% html_nodes(".description") %>% html_text()
main_event_time <-
  main_web %>% html_nodes(".dtstart") %>% html_text()
main_event_time <-
  str_trim(as.character(
    tapply(
      main_event_time,
      1:length(main_event_time),
      str_replace_all,
      "[\r\t\n]",
      ""
    )
  ))
main_event_time <- toupper(main_event_time)

Events_main <-
  data.frame(
    "Title" = main_event,
    "Location" = main_loc,
    "Content" = main_event_content,
    "Time" = main_event_time,
    "Link" = main_link,
    stringsAsFactors = FALSE
  )
Events_main$Time <-
  gsub("(^[^:][0-9]{0,2})([AP]{1}M)", "\\1:00\\2", Events_main$Time)
Events <- Events_main
Events$Time <- as.POSIXct(Events$Time, format = '%I:%M %p')

similar <- function(user_input, contents) {
  contents <-
    gsub("\\s+", " ", tolower(str_replace_all(contents, "[:,\\.\\\n\t]", " ")))
  content_bag <- unlist(str_split(contents, " "))
  res = 0
  for (i in 1:length(user_input)) {
    for (j in 1:length(content_bag)) {
      if (user_input[i] == content_bag[j] & user_input[i] != "") {
        res = res + 1
      }
    }
  }
  return(res)
}

function(input, output) {
  Input <- reactive({
    user_input <-
      tolower(c(
        input$Keywords1,
        input$Keywords2,
        input$Keywords3,
        input$Keywords4
      ))
    Events$weight <- rep(0, nrow(Events))
    for (i in 1:nrow(Events)) {
      Events$Weight[i] <- similar(user_input, Events$Content[i])
    }
    return(Events)
  })
  
  #This is setting for Bubble chart graph
  output$plot <- renderPlotly({
    plot_ly(
      Input() ,
      x = ~ Time,
      y = ~ Weight,
      type = 'scatter',
      mode = 'markers',
      size = ~ Weight,
      color = ~ Title,
      colors = 'Paired',
      sizes = c(40, 150),
      marker = list(opacity = 0.7, sizemode = 'diameter'),
      hoverinfo = 'text',
      text = ~ paste('Title:', Title, '<br>Time:', Time)
    ) %>%
      layout(
        title = '',
        xaxis = list(showgrid = TRUE),
        yaxis = list(showgrid = TRUE),
        showlegend = FALSE
      )
  })
  
  #This is table for the data output
  output$table <- renderDataTable({
    datatable(
      Input()[c("Title", "Location", "Time", "Link", "Weight")],
      options = list(
        authoWidth = TRUE,
        columnDefs = list(list(
          width = '200px', targets = c(1, 3)
        )),
        order = list(list(5, "dec"))
      ) ,
      extensions = 'Responsive',
      escape = FALSE
    )
  })
}