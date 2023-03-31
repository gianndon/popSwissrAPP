library(shiny)
library(popSwissr)
library(remotes)
library(PortfolioAnalytics)
library(plotly)
library(quantmod)
library(highcharter)
library(ggplot2)
library(dplyr)
library(scales)
library(ggrepel)
library(tidyverse)
library(TTR)

#function to abbreviate numbers
abbreviate2 <- function(x) {
  if (x >= 1e9) {
    paste0(round(x/1e9, 1), "Mrd.")
  } else if (x >= 1e6) {
    paste0(round(x/1e6, 1), "Mio.")
  } else if (x >= 1e3) {
    paste0(round(x/1e3, 1), "K")
  } else {
    as.character(x)
  }
}

# Define the assets and their expected returns and covariance matrix
# assets <- c("smi", "gold", "bitcoin", "ch_gov_bonds", "us_gov_bonds", "sp500", "usd_chf")
# returns <- c(0.06, 0.01, 0.2, 0.02, 0.03, 0.08, 0.01)
# covariance <- matrix(c(0.04, -0.01, 0.05, 0.01, 0.02, 0.03, -0.01,
#                        -0.01, 0.03, 0.02, 0.01, -0.02, -0.01, 0.03,
#                        0.05, 0.02, 0.1, -0.03, -0.01, 0.04, -0.02,
#                        0.01, 0.01, -0.03, 0.04, 0.02, -0.02, 0.01,
#                        0.02, -0.02, -0.01, 0.02, 0.05, 0.01, -0.01,
#                        0.03, -0.01, 0.04, -0.02, 0.01, 0.09, -0.02,
#                        -0.01, 0.03, -0.02, 0.01, -0.01, -0.02, 0.04), nrow = 7, ncol = 7)

# Specify the assets and date range
assets <- c("^SSMI", "USDCHF=X", "^GSPC")
smi <- NULL;




#smi_data <- getSymbols("^SSMI", auto.assign = FALSE)

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
                                   #create Donut Plot of portfolio
                                   plotOutput("donut_index", height= "65vh"),
                                   
                                   #create input for portfolio with input field
                                   column(3,numericInput("smi", "SMI Index [CHF]", value = 20000)),
                                   column(3,numericInput("ch_gov_bonds", "CH-Staatsanleihen [CHF]", value = 2000)),
                                   column(3,numericInput("gold", "Gold [CHF]", value = 2000)),
                                   column(3,numericInput("bitcoin", "Bitcoin [CHF]", value = 18000)),
                                   column(3,numericInput("us_gov_bonds", "US Staatsanleihen [CHF]", value = 5000)),
                                   column(3,numericInput("sp500", "SP500 [CHF]", value = 1000)),
                                   column(3,numericInput("usd_chf", "USD/CHF Devisen [CHF]", value = 9000)),
                                   fluidRow()),
                          tabPanel("Kursübersicht",
                                   #fluidRow(),
                                   titlePanel("Kursübersicht für SMI, S&P500, Gold,..."),
                                   #sidebarLayout(
                                     #sidebarPanel(
                                       # selectInput("period", "Select period:",
                                          #         choices = c("1 day", "1 week", "1 month", "1 year", "5 years", "10 years"))
                                       column(6, selectInput("assets", "Select Assets:", choices = c("^SSMI", "USDCHF=X", "^GSPC"), selected = c("^SSMI", "^GSPC"), multiple = TRUE)),
                                       column(6,selectInput("period", "Select period:",
                                                              choices = c("1 day", "1 week", "1 month", "1 year", "5 years", "10 years"), selected = "1 month")),
                                       #plotOutput("stockPlot")
                                       verbatimTextOutput("click_kursuebersicht2"),
                                       plotOutput("smi_plot", click="click_kursuebersicht1"),
                                       fluidRow()
                                        ),
                                    
                                     #plotOutput("smi_plot")
                                   )
                                #))
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
                          tabPanel("Individuelles Investment"),
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
 
  

# Output start date -------------------------------------------------------
start_date_selector <- reactive({
  period <- input$period
  
  # Get start and end dates based on period
  start_date <- Sys.Date() - switch(period,
                                    "1 day" = 1,
                                    "1 week" = 7,
                                    "1 month" = 30,
                                    "1 year" = 365,
                                    "5 years" = 5*365,
                                    "10 years" = 10*365)
  return(start_date)

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
                                      "5 years" = 5*365,
                                      "10 years"=10*365)
    end_date <- Sys.Date()
    
    # Filter smi_data based on start and end dates
    smi_data_filtered <- smi_data[paste(start_date, "/", end_date, sep = "")]
    
    # Return filtered smi_data
    return(smi_data_filtered)
  })
  

  #Output wenn klicken in Kursübersicht
  output$click_kursuebersicht2 = renderPrint({
    if(is.null(input$click_kursuebersicht1$x)){
      datum <- Sys.Date()
      datum1 <- datum+1
    }
    else{ 
    datum <- as.character(as.Date(input$click_kursuebersicht1$x))
    datum1 <- as.character(as.Date(datum)+1)
    }
    smi <- getSymbols("^SSMI", from = datum, to= datum1, auto.assign = FALSE )
    c("Datum" = datum
      , "SMI" = round(smi[datum,"SSMI.Adjusted"],3)
      )
    
  })
  
  
  
  output$smi_plot <- renderPlot({
    # Set background color to transparent
   
    # Plot smi_data based on selected period
   # chartSeries(smi_data_reactive(), TA = NULL, theme="white")
    
    
    
    start_date <- start_date_selector()
    end_date <- Sys.Date()+1
    
    # Get stock price data
    stock_data <- do.call(cbind,
                          lapply(assets, function(x) {
                            getSymbols(x,
                                       src = "yahoo",
                                       from = start_date,
                                       to = end_date,
                                       auto.assign = FALSE)[, 6]
                          }))
    
    # Convert to data frame
    stock_data <- data.frame(date = index(stock_data), coredata(stock_data))
    names(stock_data)[-1] <- assets
    
    # Reshape data
    stock_data <- stock_data %>%
      pivot_longer(-date, names_to = "asset", values_to = "price")
    # Filter data based on selected assets
    data <- stock_data[stock_data$asset %in% input$assets, ]
    
    # Create ggplot object
    gg <- ggplot(data, aes(x = date, y = price, color = asset)) +
      geom_line()+
      theme(panel.background = element_blank(),
            panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA))
    #if(input$click_kursuebersicht1$x %in% smi[1,]){
      # gg <- gg +
      #   geom_vline(xintercept = as.numeric("2023-03-28"),size=10, colour="red")
    #}
    
    gg
    
    # gg_ly <- ggplotly(gg)
    # gg_ly
  }, bg="transparent"  )
  
  
  
  
  
  #create Donut Plot of portfolio
  output$donut_index <- renderPlot({
    #create data frame
    df_donut <- data.frame(value=c(input$smi, input$gold, input$bitcoin, input$ch_gov_bonds, input$us_gov_bonds, input$sp500, input$usd_chf),
                           Anlage=c("SMI", "Gold", "Bitcoin", "Schweizer Staatsanleihen", "US Staatsanleihen", "S&P500", "USD/CHF"))
    # Hole size
    hsize <- 7
    
    fraction <- df_donut$value /sum(df_donut$value)
    
    
    df_donut <- df_donut %>% 
      mutate(x = hsize,
             csum = rev(cumsum(rev(value))), 
             pos = value/2 + lead(csum, 1),
             pos = if_else(is.na(pos), value/2, pos))
    
    ggplot(df_donut, aes(x = hsize, y = value, fill = Anlage)) +
      geom_col(color = "black") +
      coord_polar(theta = "y") +
      
      geom_label_repel(aes(label = percent(fraction),
                                 ),
                 position = position_stack(vjust=0.5),
                 inherit.aes = TRUE,
                 show.legend=FALSE) +
      theme(legend.title = element_text(colour = "black", size = 50, face = "bold"), 
            legend.text = element_text(colour = "black", size = 40), 
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "bottom") +
      annotate("text", x = 0, y = 0, size = 16, label = paste(abbreviate2(sum(df_donut$value)), "CHF") )+
      theme_void()
  }, bg="transparent")
  
  
  
  
}
enableBookmarking(store = "url")
shinyApp(ui, server)
