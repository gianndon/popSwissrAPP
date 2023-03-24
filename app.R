library(shiny)
library(popSwissr)

ui <- function(request) {
  
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    
    #Navbar
    navbarPage("Swisscon Portfolio", id ="navbar",
               tabPanel("Übersicht"                      ),
               tabPanel("Investment", 
                        tabsetPanel(
                          tabPanel("Rendite Maximieren / Risiko Minimieren"),
                          tabPanel("Minimum Varianz Portfolio", id="navbar"),
                          tabPanel("Tangentialportfolio"),
                          tabPanel("Kennzahlen")
                        )),
               tabPanel("Über uns", 
                        tabsetPanel(
                          tabPanel("Geschichte"),
                          tabPanel("Kontakt")
                        ))
    ),
    plotOutput("plot"),
    sliderInput("n", "Number of observations", 1, nrow(faithful), 100),
    bookmarkButton()
  )
}

server <- function(input, output, session) {
  output$plot <- renderPlot({
    f(x=input$n)
  })
}

enableBookmarking(store = "url")
shinyApp(ui, server)
