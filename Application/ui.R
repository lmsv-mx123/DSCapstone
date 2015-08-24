library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Next Word Predictor"),
  
  fluidRow(HTML("<strong>Author: Luis Salazar</strong>") ),
  fluidRow(HTML("<strong>Date: July 7th, 2015</strong>") ),
  
  fluidRow(
    br(),
    p("This Shiny application uses a smoothed Back-off algorithm to predict the next word, based on data frames of five, four, three, two and one grams from trained corpora
      of news, blogs and twitter entries. Please note that slangs, bad words, links, etc will not be taken into consideration to predict the next word.")),
  br(),
  br(),
  
  fluidRow(HTML("<strong>Enter a phrase. Press \"Next Word\" button to predict the next word</strong>") ),
  fluidRow( p("\n") ),
  
  # Sidebar layout
  sidebarLayout(
    
    sidebarPanel(
      textInput("inputText", "Enter the phrase here",value = ""),
      submitButton("Next Word")
    ),
    
    mainPanel(
      h4("Predicted Next Word"),
      verbatimTextOutput("prediction"),
      textOutput('text1'),
      textOutput('text2')
    )
  )
))