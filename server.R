
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  # Will hold the text output, depending on whether the random button or match button was pressed
  t <- reactiveValues(output = NULL)
  
  observeEvent(input$goButton, {
    searchPhrase <- gsub("[^a-zA-Z ]", "\\.", input$phrase)
    rows <- grep(searchPhrase, allComments$text2, ignore.case = TRUE)
    if(!length(rows)) {
      t$output <- "No Comments Found"
    } else {
      if(length(rows) == 1) {
        i <- rows
      } else {
        i <- sample(rows, 1)  
      }
      text <- allComments$text2[i]
      datestr <- format(allComments$timestamp[i], "%B %d, %Y at %I:%M %P")
      text <- gsub(input$phrase, paste0('<strong>', input$phrase, "</strong>"), text, ignore.case = TRUE)
      t$output <- list(
        "line1" = paste("<h3>", "Comment made by", allComments$commenter[i], "on", datestr, "</h3>"),
        "line2" = paste("<h4>", "On Post", "<em>", allComments$post[i], "</em>by", allComments$author[i], "</h4>"),
        "line3" = paste("<p>", text, "</p>")
      )
    }    
  })
  
  observeEvent(input$random, {
    i <- sample(1:nrow(allComments), 1)
    text <- allComments$text[i]
    text <- gsub(input$phrase, paste0("<strong>", input$phrase, "</strong>"), text, ignore.case = TRUE)
    datestr <- format(allComments$timestamp[i], "%B %d, %Y at %I:%M %P")
    t$output <- list("line1" = paste("<h3>Comment made by", allComments$commenter[i], "on", datestr, "</h3>"),
         "line2" = paste("<h4>", "On Post", "<em>", allComments$post[i], "</em>by", allComments$author[i], "</h4>"), 
         "line3" = paste("<p>", text, "</p>"))
  })
  
  observeEvent(input$timeButton, {
    showModal(
      modalDialog(
        plotOutput("timePlot")
      )
    )
  })
  
  output$timePlot <- renderPlot({
    p
  }, res = 112)

  output$foundText <- renderUI({
    if(is.null(t$output)) {
      return(c())
    }
    toutpt <- t$output
    if(toutpt[[1]] == "No Comments Found") {
      HTML("No Comments Found")
    } else {
      # HTML(paste(toutpt[[1]], toutpt[[2]], toutpt[[3]], sep = '<br/>'))
      HTML(paste(toutpt[[1]], toutpt[[2]], toutpt[[3]])) 
    }
  })
  
})
