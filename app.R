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
library(RColorBrewer)
#devtools::install_github("gianndon/popSwissr")

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
assets <- c("^SSMI", "USDCHF=X", "^GSPC", "GC=F", "BTC-USD", "CSBGC0.SW", "^TNX")
                #c("SMI", "USD / CHF", "S&P500", " ", "Bitcoin USD", " ", " ", " "))
assets1 <- c("SMI"="^SSMI", "USD/CHF"="USDCHF=X", "S&P500"="^GSPC", "Gold"="GC=F", "Bitcoin USD"="BTC-USD", "CH Staatsanleihen"="CSBGC0.SW", "US Staatsanleihen"="^TNX")

title <- tags$a(tags$img(src="Swisscon_Logo2.png", height="45px", id="logo"))





#smi_data <- getSymbols("^SSMI", auto.assign = FALSE)

# Define the user interface
ui <- function(request) {
  
  fluidPage(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    
    #Navbar
    navbarPage(title=title,
               id="navbar", fluid=TRUE,
               tabPanel("Übersicht",
                        tabsetPanel(
                          tabPanel("Portfolio",
                                   mainPanel(
                                   column(9, h2("Portfolio")),
                                   column(2,actionButton("new_investment", "Neues Portfolio anlegen")),
                                   
                                   #create Donut Plot of portfolio
                                   plotOutput("donut_index", height= "65vh"),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   #create input for portfolio with input field
                                   column(3,numericInput("smi", "SMI Index [CHF]", value = 20000)),
                                   column(3,numericInput("ch_gov_bonds", "CH-Staatsanleihen [CHF]", value = 2000)),
                                   column(3,numericInput("gold", "Gold [CHF]", value = 2000)),
                                   column(3,numericInput("bitcoin", "Bitcoin [CHF]", value = 18000)),
                                   column(3,numericInput("us_gov_bonds", "US Staatsanleihen [CHF]", value = 5000)),
                                   column(3,numericInput("sp500", "SP500 [CHF]", value = 1000)),
                                   column(3,numericInput("usd_chf", "USD/CHF Devisen [CHF]", value = 9000)),
                                   column(3, actionButton("load-donut-chart", "Chart aktualisieren")),
                                   br(),
                                   br(),
                                   fluidRow(), 
                                   width ="100vh")),
                            
                          tabPanel("Kursübersicht",
                                   dataTableOutput("test"),
                                   #fluidRow(),
                                   titlePanel("Kursübersicht für SMI, S&P500, Gold,..."),
                                   #sidebarLayout(
                                     #sidebarPanel(
                                       # selectInput("period", "Select period:",
                                          #         choices = c("1 day", "1 week", "1 month", "1 year", "5 years", "10 years"))
                                       column(6, checkboxGroupInput("assets2", "Select Assets:", choices = assets1, 
                                                             selected = c("^SSMI", "^GSPC"),
                                                             inline=TRUE,
                                                             #multiple = TRUE,
                                                             ), id="kursuebersicht_style"),
                                       column(6,selectInput("period", "Select period:",
                                                              choices = c("1 day", "1 week", "1 month", "1 year", "5 years", "10 years"), 
                                                              selected = "1 month"), id="kursuebersicht_style"),
                                       #plotOutput("stockPlot")
                                       
                                       plotOutput("smi_plot", click="click_kursuebersicht1"),
                                       br(),
                                       verbatimTextOutput("click_kursuebersicht2"),
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
  output$click_kursuebersicht2 <- renderPrint({
    if(is.null(input$click_kursuebersicht1)){
      datum <- Sys.Date()
      
    }
    else{
    # datum <- as.Date(input$click_kursuebersicht1$x)
    # datum1 <- as.Date(datum)+1
      click_pos <- input$click_kursuebersicht1
      datum <- as.POSIXct(input$click_kursuebersicht1$x, origin = "1970-01-01", tz = "UTC")
      datum <- format(datum, "%Y-%m-%d")
      #click_value <- data[click_date]
    }
   # smi <- getSymbols("^SSMI", from = datum, to= datum1, auto.assign = FALSE )
    # c("Datum" = datum
    #   , "SMI" = round(smi[datum,"SSMI.Adjusted"],3)
    #   )
    c("Datum: " = datum, "SMI: " = round(dataset()[,1][datum],2),
      "USD/CHF"=round(dataset()[,2][datum],2), 
      "S&P500: "= round(dataset()[,3][datum],2),
      "Gold:" = round(dataset()[,4][datum],2),
      "Bitcoin: " = round(dataset()[,5][datum],2),
      "CH Staatsanleihen:" = round(dataset()[,6][datum],2),
      "US Staatsanleihen:" = round(dataset()[,7][datum],2))
    #print(input$click_kursuebersicht1)
  })
  
  output$test <- renderDataTable(rownames(dataset1()))
  
  dataset <- reactive({
    # Plot smi_data based on selected period
    # chartSeries(smi_data_reactive(), TA = NULL, theme="white")
    
    
    
    start_date <- start_date_selector()
    end_date <- Sys.Date()
    
    popSwissr::get_data(assets, start_date, end_date)
    
    #popSwissr::convert_currencies(assets)
    
    # Get stock price data
    # do.call(cbind, 
    #       lapply(assets, function(x) {
    #         getSymbols(x,
    #             src = "yahoo",
    #             from = start_date,
    #             to = end_date,
    #             auto.assign = FALSE)[, 6]
    #                       }))
    
    # # Convert to data frame
    # stock_data <- data.frame(date = index(stock_data), coredata(stock_data))
    # names(stock_data)[-1] <- assets
    # 
    # Reshape data
    # stock_data <- stock_data %>%
    #   pivot_longer(-date, names_to = "asset", values_to = "price")
    # # Filter data based on selected assets
    # data <- stock_data[stock_data$asset %in% input$assets, ]
  })
  
  #full dataset with all data
  dataset1 <- reactive({
    temp <- dataset()
    colnames(temp) <- assets
    temp
  })
  
  #dataset with selected data in Kursübersicht
  dataset2 <- reactive({
    temp <- dataset()
    colnames(temp) <- assets
    temp
    
    # subset data based on selected assets
    subset_data <- temp[, input$assets2]
    
    # return xts object
    xts(subset_data, order.by = index(temp))
    
  })
  
  output$smi_plot <- renderPlot({
    
    coulours <- brewer.pal(8, "Dark2")
    plot.xts(dataset2(), bg="transparent", col=coulours, col.lab="gold2", labels.col="navyblue", cex.axis=1.3 , lwd=3)
    addLegend("topleft", lty=1, lwd=2)
    
    # Create ggplot object
    # gg <- ggplot(dataset1(), aes(x = as.data.frame(index(dataset1())), y = as.data.frame(coredata(dataset())), color = assets)) +
    #   geom_line()+
    #   theme(panel.background = element_blank(),
    #         panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    #         panel.grid.minor = element_blank(),
    #         plot.background = element_rect(fill = "transparent", color = NA))
    #if(input$click_kursuebersicht1$x %in% smi[1,]){
      # gg <- gg +
      #   geom_vline(xintercept = as.numeric("2023-03-28"),size=10, colour="red")
    #}
    
    #gg
    
    # gg_ly <- ggplotly(gg)
    # gg_ly
  }, bg="transparent"  )
  
  reactive(print(dataset1()))
  
  
  
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
      geom_col(color = "black", size=1) +
      scale_fill_brewer(palette = "YlGnBu") + # change fill to gold color palette
      coord_polar(theta = "y") +
      
      geom_label_repel(aes(label = paste(percent(fraction)), 
                                 ),
                 position = position_stack(vjust=0.5),
                 inherit.aes = TRUE,
                 show.legend=FALSE,
                 box.padding = 0,
                 size=5, 
                 color="gold3"  ) +
      guides(fill = guide_legend(title = "Anlage", title.position = "top"))+
      # theme(legend.title = element_text(color = "gold", size = 50, face = "bold"), 
      #       legend.text = element_text(color = "gold", size = 40), 
      #       panel.grid = element_blank(),
      #       axis.text = element_blank(),
      #       axis.title = element_blank(),
      #       axis.ticks = element_blank(),
      #       plot.title=element_blank(),
      #       legend.position = "bottom") +
      annotate("text", x = 0, y = 0, size = 20, color="navyblue", label = paste(abbreviate2(sum(df_donut$value)), "CHF"))+
      theme_void()
  }, bg="transparent")
  
  observeEvent(input$new_investment, {
    updateNavbarPage(session, "navbar", selected="Investment")
  })
  
  
}
enableBookmarking(store = "url")
shinyApp(ui, server)
