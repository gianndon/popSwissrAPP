library(shiny)
library(popSwissr)

ui <- function(request) {
  fluidPage(
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
