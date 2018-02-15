

library(shiny)

shinyUI(fluidPage(

  titlePanel("Shiny App Search engine"),
  sidebarLayout(
    sidebarPanel(
      textInput("Phrase","Enter search phrase",value = "hong kong is the an asian markets"),
      submitButton(text="search")
    ),
    
    mainPanel(
      
      h4("Results:")  ,
      verbatimTextOutput("result"),
      h4("summary:"),
      dataTableOutput('func')
      
              )
  )
))
