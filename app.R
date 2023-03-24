library(shiny)
library(popSwissr)
library(remotes)
library(PortfolioAnalytics)

# Define the assets and their expected returns and covariance matrix
assets <- c("smi", "gold", "bitchoin", "ch_gov_bonds", "us_gov_bonds", "sp500", "usd_chf")
returns <- c(0.06, 0.01, 0.2, 0.02, 0.03, 0.08, 0.01)
covariance <- matrix(c(0.04, -0.01, 0.05, 0.01, 0.02, 0.03, -0.01,
                       -0.01, 0.03, 0.02, 0.01, -0.02, -0.01, 0.03,
                       0.05, 0.02, 0.1, -0.03, -0.01, 0.04, -0.02,
                       0.01, 0.01, -0.03, 0.04, 0.02, -0.02, 0.01,
                       0.02, -0.02, -0.01, 0.02, 0.05, 0.01, -0.01,
                       0.03, -0.01, 0.04, -0.02, 0.01, 0.09, -0.02,
                       -0.01, 0.03, -0.02, 0.01, -0.01, -0.02, 0.04), nrow = 7, ncol = 7)

# Define the user interface
ui <- function(request) {
  
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    
    #Navbar
    navbarPage("Swisscon Portfolio", id="navbar", fluid=TRUE, 
               tabPanel("Übersicht",
                        column(3,numericInput("smi", "SMI Index [CHF]", value = 0)),
                        column(3,numericInput("ch_gov_bonds", "CH-Staatsanleihen [CHF]", value = 0)),
                        column(3,numericInput("gold", "Gold [CHF]", value = 0)),
                        column(3,numericInput("bitcoin", "Bitcoin [CHF]", value = 0)),
                        column(3,numericInput("us_gov_bonds", "US Staatsanleihen [CHF]", value = 0)),
                        column(3,numericInput("sp500", "SP500 [CHF]", value = 0)),
                        column(3,numericInput("usd_chf", "USD/CHF Devisen [CHF]", value = 0)),
                        fluidRow(),
                        h2("Portfolio"),
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
    titlePanel(h2("Test")),
    plotOutput( "plot"),
    sliderInput("n", "Number of observations", 1, nrow(faithful), 100),
    bookmarkButton()
  )
}

# Define the server logic
server <- function(input, output, session) {
  observeEvent(input$submit, {
    smi <- input$SMI
    ch_gov_bonds <- input$Ch-Staatsanleihen
    gold <- input$gold
    bitcoin <- input$bitcoin
    us_gov_bonds <- input$us-staatsanleihen
    sp500 <- input$sp500
    usd_chf <- input$usd-chf-devisen
    
  })
  output$plot <- renderPlot({
    #change color of plot background
    par(bg = "#f3f4fa")
    f(x=input$n)
    
  })
  observeEvent(input$optimize_button,{
    # Create a portfolio object
my_portfolio <- portfolio.spec(assets)

# Set the expected returns for each asset
setTargetReturn(my_portfolio) <- mean(returns)

# Add constraints (e.g. no short-selling)
add.constraint(my_portfolio, type = "longonly")

# Add the covariance matrix of asset returns
setcov(my_portfolio) <- cov_matrix

# Calculate the minimum variance portfolio
min_var_portfolio <- optimize.portfolio(
  my_portfolio,
  portfolio.optimization = "minvariance",
  trace = FALSE
)

# Print the weights of the minimum variance portfolio
print(min_var_portfolio$weights)
  })
}

enableBookmarking(store = "url")
shinyApp(ui, server)
