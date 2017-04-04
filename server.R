library(datasets)
library(plotly)
library(shiny)
library(DT)
library(RXKCD)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(rvest)
library(stringr)
library(methods)
options(DT.fillContainer = FALSE) 
options(DT.autoHideNavigation = FALSE)

# str_replace_all(x, "[\r\n]" , "")

# Lyon Center
# lyon_url <- 'https://sait.usc.edu/recsports/calendars/list/'
# lyon_web <- read_html(lyon_url)
# lyon_event <- lyon_web %>% html_nodes(".tribe-events-content")
# 
# lyon_event_date <- lyon_web %>% html_nodes(".tribe-event-date-start")
# lyon_event_title <- lyon_web %>% html_nodes(".tribe-event-url") %>% html_text()
# lyon_event_content <- lyon_event %>% html_text()
# lyon_event_content <- as.character(tapply(lyon_event_content, 1:length(lyon_event_content),str_replace_all, "[\r\t\n]", ""))
# lyon_event_date <- lyon_event_date %>% html_text()
# lyon_event_loc <- rep("Lyon Center", length(lyon_event_content)) # generate location
# 
# # thornton Center
# thorn_url <- 'https://music.usc.edu/events/'
# thorn_web <- read_html(thorn_url)
# thorn_event <- thorn_web %>% html_nodes(".event a")
# thorn_event_content <- thorn_web %>% html_nodes(".summary") %>% html_text()
# thorn_event_loc <- thorn_event %>% html_text()
# thorn_event_date <- thorn_web %>% html_nodes(".event-date-start") %>% html_text()
# thorn_event_time <- thorn_web %>% html_nodes(".event-time") %>% html_text()
# 
# # Marshall
# 
# marsh_url <- "https://www.marshall.usc.edu/news/events"
# marsh_web <- read_html(marsh_url)
# marsh_event <- marsh_web %>% html_nodes(".field-content a")
# marsh_event_content <- marsh_web %>% html_nodes(".views-field-field-teaser-value .field-content") %>% html_text()
# marsh_event_month <- marsh_event_content <- marsh_web %>% html_nodes(".date-month") %>% html_text()
# marsh_event_day <- marsh_event_content <- marsh_web %>% html_nodes(".date-day") %>% html_text()
# marsh_event_weekday <- marsh_event_content <- marsh_web %>% html_nodes(".date-weekday") %>% html_text()
# marsh_event_date <- paste(marsh_event_month , marsh_event_day, ",",marsh_event_weekday)

# Main 

main_url <- "https://calendar.usc.edu/calendar/day"
main_web <- read_html(main_url)
main_event <- main_web %>% html_nodes(".summary a") %>% html_text()
main_link <- as.character(main_web %>% html_nodes(".summary a")) # %>% html_attr("href")
main_loc <- main_web %>% html_nodes(".event_item_venue") %>% html_text()
main_event_content <- main_web %>% html_nodes(".description") %>% html_text()
main_event_time <- main_web %>% html_nodes(".dtstart") %>% html_text()
main_event_time <- str_trim(as.character(tapply(main_event_time, 1:length(main_event_time),str_replace_all, "[\r\t\n]", "")))
main_event_time <- toupper(main_event_time)

viterbi_url <- "https://viterbi.usc.edu/calendar/"
viterbi_web <- read_html(viterbi_url)
viterbi_event <- viterbi_web %>% html_nodes("h3 a") %>% html_text()
viterbi_link <- paste("<a href= 'https://viterbi.usc.edu/calendar/",viterbi_web %>% html_nodes("h3 a") %>% html_attr("href"), "'>",viterbi_event,"</a>",sep ='')
viterbi_event_content <- viterbi_web %>% html_nodes("blockquote") %>% html_text()
viterbi_time <- viterbi_web %>% html_nodes("strong") %>% html_text()
viterbi_time <- lapply(str_split(lapply(str_split(viterbi_time, "@"),`[`, 2), "-"), `[`, 1)
viterbi_time <- gsub(" ","",viterbi_time)
viterbi_loc <- viterbi_web %>% html_nodes("blockquote+ p a") %>% html_text()

Events_main <- data.frame("Title" = main_event, "Location" = main_loc, "Content" = main_event_content,
                          "Time" = main_event_time, "Link" = main_link, stringsAsFactors=FALSE)
Events_main$Time <- gsub("(^[^:][0-9]{0,2})([AP]{1}M)","\\1:00\\2",Events_main$Time)
Events_viterbi <- data.frame("Title" = viterbi_event, "Location" = viterbi_loc, "Content" = viterbi_event_content,
                             "Time" = viterbi_time, "Link" = viterbi_link, stringsAsFactors=FALSE)
Events <- rbind(Events_main,Events_viterbi)

Events$Time <- as.POSIXct(Events$Time, format = '%I:%M %p')

ap.corpus <- Corpus(DataframeSource(data.frame(Events["Content"])))
ap.corpus <- tm_map(ap.corpus, removePunctuation)
ap.corpus <- tm_map(ap.corpus, content_transformer(tolower))
ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, stopwords("english")))
ap.tdm <- TermDocumentMatrix(ap.corpus)
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
table(ap.d$freq)
pal2 <- brewer.pal(8,"Dark2")

similar <- function(user_input, contents){
  contents <- gsub("\\s+", " ",tolower(str_replace_all(contents, "[:,\\.\\\n\t]", " ")))
  content_bag <- unlist(str_split(contents, " "))
  res = 0
  for(i in 1:length(user_input)){
    for(j in 1:length(content_bag)){
      if(user_input[i] == content_bag[j] & user_input[i] != ""){
        res = res + 1
      }
    }
  }
  return(res)
}

function(input, output) {
  
  Input <- reactive({
    user_input <- tolower(c(unlist(str_split(input$Keywords, " "))))
    Events$weight <- rep(0,nrow(Events))
      for(i in 1:nrow(Events)){
        Events$Weight[i] <- similar(user_input,Events$Content[i])
      }
    return(Events)
  }
  )
  
  output$wordcloud <- renderPlot(
    {
      wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,
                max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
    }
  )
  
  #This is setting for Bubble chart graph
  output$plot <- renderPlotly({
    plot_ly(Input() , x = ~Time, y = ~Weight, type = 'scatter', mode = 'markers', size = ~Weight, color = ~Title, colors = 'Paired',
            sizes = c(40,150),
            marker = list(opacity = 0.7, sizemode = 'diameter'),
            hoverinfo = 'text',
            text = ~paste('Title:', Title, '<br>Time:', Time))%>%
      layout(title = '',
             xaxis = list(showgrid = TRUE),
             yaxis = list(showgrid = TRUE),
             showlegend = FALSE)
  })
  
  #This is table for the data output
  output$table <- renderDataTable({
    datatable(Input()[c("Title","Location","Time","Link","Weight")], options = list(authoWidth = TRUE, 
                                                                                   columnDefs = list(list(width = '200px', targets = c(1, 3))),
                                                                                   order = list(list(5,"dec"))) , extensions ='Responsive', escape = FALSE)
  })
}