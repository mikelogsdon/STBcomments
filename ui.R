
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Seattle Transit Blog Comments"),

  # Sidebar with action buttons and text box
  sidebarLayout(
    sidebarPanel(
      actionButton("random", "Random Comment"),
      textInput("phrase", "Or Enter a Word or Phrase\n(Tacoma Link, Subarea Equity, First Hill Station, You're Wrong, Boondoggle, ...)"),
      actionButton("goButton", "Get Another Match"),
      actionButton("timeButton", "When Are Commenters Active?")
    ),

    # print the comment
    mainPanel(
      htmlOutput("foundText")
    )
  )
))
