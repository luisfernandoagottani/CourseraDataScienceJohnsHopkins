library(shiny)
shinyUI(fluidPage(
    titlePanel("Next word prediction App - Capstone Project JHDCC"),
    sidebarLayout(
        sidebarPanel(
            textAreaInput("textInput", 
                          label = "Box text, write here to check out the next word prediction:", 
                          value = "", 
                          width = "100%", 
                          rows = 6)
        ),
        mainPanel(
            h3("Probably your next word is:"),
            fluidRow(column(10, verbatimTextOutput("predictedWord", placeholder = TRUE)))
        )
    )
))

