library(shiny)
library(popSwissr)

ui <- function(request) {
  
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    
    #Navbar
    navbarPage("Swisscon Portfolio", id="navbar", fluid=TRUE, 
               tabPanel("Übersicht"  ),
               tabPanel("Investment",  
                        tabsetPanel(id="tabsetPanel",
                          tabPanel("Rendite Maximieren / Risiko Minimieren"),
                          tabPanel("Minimum Varianz Portfolio"),
                          tabPanel("Tangentialportfolio"),
                          tabPanel("Kennzahlen")
                        )),
               tabPanel("Über uns", 
                        tabsetPanel(
                          tabPanel("Geschichte"),
                          tabPanel("Kontakt")
                        ))
    ),
    titlePanel(h2("Test")),
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
