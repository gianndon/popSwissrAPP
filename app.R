library(shiny)
library(popSwissr)
library(remotes)
library(PortfolioAnalytics)
library(plotly)
library(quantmod)
library(highcharter)

# Define the assets and their expected returns and covariance matrix
assets <- c("smi", "gold", "bitcoin", "ch_gov_bonds", "us_gov_bonds", "sp500", "usd_chf")
returns <- c(0.06, 0.01, 0.2, 0.02, 0.03, 0.08, 0.01)
covariance <- matrix(c(0.04, -0.01, 0.05, 0.01, 0.02, 0.03, -0.01,
                       -0.01, 0.03, 0.02, 0.01, -0.02, -0.01, 0.03,
                       0.05, 0.02, 0.1, -0.03, -0.01, 0.04, -0.02,
                       0.01, 0.01, -0.03, 0.04, 0.02, -0.02, 0.01,
                       0.02, -0.02, -0.01, 0.02, 0.05, 0.01, -0.01,
                       0.03, -0.01, 0.04, -0.02, 0.01, 0.09, -0.02,
                       -0.01, 0.03, -0.02, 0.01, -0.01, -0.02, 0.04), nrow = 7, ncol = 7)

smi_data <- getSymbols("^SSMI", auto.assign = FALSE)

# Define the user interface
ui <- function(request) {
  
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    
    #Navbar
    navbarPage("Swisscon Portfolio", id="navbar", fluid=TRUE, 
               tabPanel("Übersicht",
                        tabsetPanel(
                          tabPanel("Portfolio",
                                   h2("Portfolio"),
                                   column(3,numericInput("smi", "SMI Index [CHF]", value = 0)),
                                   column(3,numericInput("ch_gov_bonds", "CH-Staatsanleihen [CHF]", value = 0)),
                                   column(3,numericInput("gold", "Gold [CHF]", value = 0)),
                                   column(3,numericInput("bitcoin", "Bitcoin [CHF]", value = 0)),
                                   column(3,numericInput("us_gov_bonds", "US Staatsanleihen [CHF]", value = 0)),
                                   column(3,numericInput("sp500", "SP500 [CHF]", value = 0)),
                                   column(3,numericInput("usd_chf", "USD/CHF Devisen [CHF]", value = 0)),
                                   fluidRow()),
                          tabPanel("Übersicht",
                                   fluidRow(),
                                   titlePanel("Swiss Market Index (SMI)"),
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput("period", "Select period:",
                                                   choices = c("1 day", "1 week", "1 month", "1 year", "5 years"))
                                     ),
                                     
                                     plotOutput("smi_plot")
                                   )))
                          ),
                        
                      tabPanel("Investment",  
                        tabsetPanel(id="tabsetPanel",
                          tabPanel("Rendite Maximieren / Risiko Minimieren"),
                          tabPanel("Minimum Varianz Portfolio",
                                   actionButton("optimize_button", "Optimieren"),
                                   h3("Portfolio-Ergebnisse:"),
                                   verbatimTextOutput("portfolio_results")
                                   
                                   ),
                          tabPanel("Tangentialportfolio"),
                          tabPanel("Kennzahlen")
                        )),
               tabPanel("Über uns", 
                        tabsetPanel(
                          tabPanel("Geschichte"),
                          tabPanel("Kontakt")
                        ))
    ),
    #titlePanel(h2("Test")),
    #plotOutput( "plot"),
    #sliderInput("n", "Number of observations", 1, nrow(faithful), 100),
    #bookmarkButton()
  )
}

# Define the server logic
server <- function(input, output, session) {
  

# Eingabe aktuelles Portfolio ---------------------------------------------
  observeEvent(input$submit, {
    smi <- input$SMI
    ch_gov_bonds <- input$Ch-Staatsanleihen
    gold <- input$gold
    bitcoin <- input$bitcoin
    us_gov_bonds <- input$us-staatsanleihen
    sp500 <- input$sp500
    usd_chf <- input$usd-chf-devisen
    
  })
  

# Plot Testfunktion -------------------------------------------------------

  output$plot <- renderPlot({
    #change color of plot background
    par(bg = "#f3f4fa")
    f(x=input$n)
    
  })
 
  

# SMI Plot Slider Daten ---------------------------------------------------

  smi_data_reactive <- reactive({
    period <- input$period
    
    # Get start and end dates based on period
    start_date <- Sys.Date() - switch(period,
                                      "1 day" = 1,
                                      "1 week" = 7,
                                      "1 month" = 30,
                                      "1 year" = 365,
                                      "5 years" = 5*365)
    end_date <- Sys.Date()
    
    # Filter smi_data based on start and end dates
    smi_data_filtered <- smi_data[paste(start_date, "/", end_date, sep = "")]
    
    # Return filtered smi_data
    return(smi_data_filtered)
  })
  
  output$smi_plot <- renderPlot({
    # Plot smi_data based on selected period
    chartSeries(smi_data_reactive(), TA = NULL)
    
    'stockChart(smi_data_reactive(), type = "line", name = symbol)'
  })
  
  
}
enableBookmarking(store = "url")
shinyApp(ui, server)
