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
library(matrixStats)
library(shinyWidgets)
library(shinydashboard)
library(nloptr)
library(leaflet)
library(geosphere)
#devtools::install_github("gianndon/popSwissr", force=TRUE)





#binds data columnwise and fills with NA
cbind.fill <- function(...){
  nm <- list(...)
  nm<-lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function(x)
    rbind(x, matrix(,n-nrow(x), ncol(x))))) # ignore error
}

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




# Specify the assets and date range
assets <- c("^SSMI", "USDCHF=X", "^GSPC", "GC=F", "BTC-USD", "SREN.SW", "^TNX")
                #c("SMI", "USD / CHF", "S&P500", " ", "Bitcoin USD", " ", " ", " "))
assets1 <- c("SMI"="^SSMI", "USD/CHF"="USDCHF=X", "S&P500"="^GSPC", "Gold"="GC=F", "Bitcoin USD"="BTC-USD", "CH Staatsanleihen"="SREN.SW", "US Staatsanleihen"="^TNX")
assets2 <- c("SMI", "USD/CHF", "S&P500", "Gold", "Bitcoin USD", "CH Staatsanleihen", "US Staatsanleihen")
assets3 <- c("SMI"="^SSMI", "S&P500"="^GSPC", "Gold"="GC=F", "Bitcoin USD"="BTC-USD", "CH Staatsanleihen"="SREN.SW", "US Staatsanleihen"="^TNX")



#create new asset names
rename_assets <- function(asset){
  name <- c()
  for (i in asset) {
    
    for (j in 1:length(assets2)) {
      if(i==assets[j]){
        name<-c(name,assets2[j])
      }
    }
    
  }
  name
}

title <- tags$a(tags$img(src="Swisscon_logo1.png", height="45px", id="logo"))

#data up to 1970
data_1970 <- popSwissr::convert_currencies(assets, Sys.Date()-30*365, Sys.Date())


#smi_data <- getSymbols("^SSMI", auto.assign = FALSE)

# Define the user interface
ui <- function(request) {
  
  fluidPage(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    tags$body(
      HTML('
      <body>
  <div>
     <div class="wave"></div>
     <div class="wave"></div>
     <div class="wave"></div>
  </div>
</body>
    ')
    ),
    
    
    #Navbar
    navbarPage(title=title,
               id="navbar", fluid=TRUE,
               ###### Here : insert shinydashboard dependencies ######
               header = tagList(
                 useShinydashboard()
               ),
               #######################################################
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
                                   column(3,),
                                   br(),
                                   br(),
                                   fluidRow(),
                                   column(6, 
                                          h1("MVP"),
                                          plotOutput("boxplot_mvp_my_portfolio", height= "45vh")),
                                  column(6, 
                                         h1("TP"),
                                         plotOutput("boxplot_tp_my_portfolio", height= "45vh")),
                                   width ="100vh")),
                            
                          tabPanel("Kursübersicht",
                                   dataTableOutput("test"),
                                   #fluidRow(),
                                   titlePanel("Kursübersicht "),
                                   #sidebarLayout(
                                     #sidebarPanel(
                                       # selectInput("period", "Select period:",
                                          #         choices = c("1 day", "1 week", "1 month", "1 year", "5 years", "10 years"))
                                       column(9, checkboxGroupInput("assets2", "Select Assets:", choices = assets1, 
                                                             selected = assets1,
                                                             inline=TRUE,
                                                             #multiple = TRUE,
                                                             ), id="kursuebersicht_style"),
                                       column(3,selectInput("period", "Select period:",
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
                          tabPanel("Rendite Maximieren / Risiko Minimieren",
                                   column(9, checkboxGroupInput("assets_rendite", "Select Assets:", choices = assets3, 
                                                                selected = assets3,
                                                                inline=TRUE,
                                                                #multiple = TRUE,
                                   ), id="kursuebersicht_style"),
                                   column(3,numericInput("rendite_amount", "Amount [CHF]", value = 20000)),
                                   
                          
                                   column(6, 
                                          h1("Rendite Maximieren"),
                                          sliderInput("slider_rendite",
                                                      "Risiko",
                                                      min=0,
                                                      max=0.5,
                                                      value=0.05,
                                                      step=0.01),
                                          checkboxInput("shorting_rendite", "shorting" ),
                                          column(12, plotOutput("boxplot_rendite", height= "65vh")),
                                          
                                   ),
                                   column(6, 
                                          h1("Risiko Minimieren"),
                                          sliderInput("slider_risiko",
                                                      "Rendite",
                                                      min=0,
                                                      max=0.5,
                                                      value=0.05,
                                                      step=0.01),
                                   checkboxInput("shorting_risiko", "shorting" ),
                                   column(12, plotOutput("boxplot_risiko", height= "65vh")),
                                   
                          )),
                          
                          tabPanel("Minimum Varianz Portfolio",
                                   h2("Minimum Varianz Portfolio"),
                                   column(9, checkboxGroupInput("assets3", "Select Assets:", choices = assets3, 
                                                                selected = assets3,
                                                                inline=TRUE,
                                                                #multiple = TRUE,
                                   ), id="kursuebersicht_style"),
                                   column(2,numericInput("mvp_amount", "Amount [CHF]", value = 20000)),
                                   column(1,checkboxInput("shorting_mvp", "shorting" )),
                                   
                                   br(),
                                   column(12, plotOutput("donut_mvp", height= "65vh")),
                                   br(),
                                   fluidRow(valueBoxOutput("mvp_renditeBox"),
                                            valueBoxOutput("mvp_risikoBox")),
                                   column(12, tableOutput("MVP_OUTPUT")),
                                 
                                   
                                   ),
                          tabPanel("Tangentialportfolio",
                                   h2("Tangential Portfolio"),
                                   column(9, checkboxGroupInput("assets5", "Select Assets:", choices = assets3, 
                                                                selected = assets1,
                                                                inline=TRUE,
                                                                #multiple = TRUE,
                                   ), id="kursuebersicht_style"),
                                  
                                   column(3,numericInput("tp_amount", "Amount [CHF]", value = 20000)),
                                   
                                   br(),
                                   column(12, plotOutput("donut_tp", height= "65vh")),
                                   br(),
                                   fluidRow(valueBoxOutput("tp_renditeBox"),
                                            valueBoxOutput("tp_risikoBox")),
                                   column(12, tableOutput("TP_OUTPUT")),
                                   
                                  
                                   ),
                          tabPanel("Individuelles Investment",
                                   h1("Individuelles Investment"),
                                     #create input for portfolio with input field
                                     column(3,numericInput("smi2", "SMI Index [CHF]", value = 20000)),
                                     column(3,numericInput("ch_gov_bonds2", "CH-Staatsanleihen [CHF]", value = 2000)),
                                     column(3,numericInput("gold2", "Gold [CHF]", value = 2000)),
                                     column(3,numericInput("bitcoin2", "Bitcoin [CHF]", value = 18000)),
                                     column(3,numericInput("us_gov_bonds2", "US Staatsanleihen [CHF]", value = 5000)),
                                     column(3,numericInput("sp5002", "SP500 [CHF]", value = 1000)),
                                     column(3,),
                                   column(12, plotOutput("boxplot_indiv", height= "65vh")),
                                   
                                   fluidRow(valueBoxOutput("indiv_renditeBox"),
                                            valueBoxOutput("indiv_risikoBox")),
                                   column(12, tableOutput("INDIV_OUTPUT")),
                                   
                                   
                                   ),
                          tabPanel("Kennzahlen",
                                   h1("Kennzahlen"),
                                   )
                        )),
               tabPanel("Über uns", 
                        tabsetPanel(
                          tabPanel("Geschichte",
                                   includeHTML("www/uber_uns.html")
                                   ),
                          tabPanel("Kontakt",
                                   includeHTML("www/kontakt.Rhtml"),
                                   leafletOutput("map"))
                        )),
               tabPanel(
                 div(
                   img(class="user_id shiny-ignore", src = "user-icon.png", height = "25px", width = "25px", style = "border-radius: 50%; "),
                   style = "margin-bottom: 0px; padding-bottom: 0px; display: flex; line-height: 0;"
                 )
               )
               
    ),
    tags$footer(
      style = "text-align: center;",
      "Copyright © 2023 Swisscon. All rights reserved."
    )
    
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
    ch_gov_bonds <<- input$us_gov_bonds
    gold <- input$gold
    bitcoin <- input$bitcoin
    us_gov_bonds <- input$us_gov_bonds
    sp500 <- input$sp500
    usd_chf <- input$usd-chf-devisen
    
  })
  
  #c("SMI"="^SSMI", "USD/CHF"="USDCHF=X", "S&P500"="^GSPC", "Gold"="GC=F", "Bitcoin USD"="BTC-USD", "CH Staatsanleihen"="SREN.SW", "US Staatsanleihen"="^TNX")
  smi_port <- reactive({input$smi})
  sp500_port <- reactive({input$sp500})
  ch_gov_bonds_port <- reactive({input$ch_gov_bonds})
  gold_port <- reactive({input$gold})
  bitcoin_port <- reactive({input$bitcoin})
  us_gov_bonds_port <- reactive({input$us_gov_bonds})
  usd_chf_port <- reactive({input$usd_chf})
  
  my_portfolio <- reactive({c(smi_port(), 
                              usd_chf_port(), 
                              sp500_port(), 
                              gold_port(), 
                              bitcoin_port() ,
                              ch_gov_bonds_port(), 
                              us_gov_bonds_port() 
                              )})
  


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
    start_date <- start_date_selector()
    end_date <- Sys.Date()
    
    #popSwissr::get_data(assets, start_date, end_date)
    
    popSwissr::convert_currencies(assets, starting_point=start_date, end_point= end_date)
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
  dataset3 <- reactive({
    temp <- data_1970
    
    assets1 <- input$assets3
    assets1 <- c(assets1, "USDCHF=X")
    # print("assets5")
    # print(assets1)
    # 
    dat_raw <- popSwissr::convert_currencies(symbols=assets1)
    dat_raw <- dat_raw[, !colnames(dat_raw) %in% "USDCHF.X.Adjusted"]
    # print("dat_raw:")
    # print(is(dat_raw))
    # print(dat_raw)
    dat <- timeSeries::returns(dat_raw)
    # print("dat:")
    # print(is(dat))
    
    colnames(dat) <- c(input$assets3)
    # print("colnames(dat)")
    # print(colnames(dat))
    dat <- na.omit(dat)
    # print(dat)
    return(dat)
    
    # colnames(temp) <- assets
    # temp
    # 
    # # subset data based on selected assets
    # subset_data <- temp[, input$assets3]
    # 
    # # return xts object
    # xts(subset_data, order.by = index(temp))
    
  })
  
  dataset4 <- reactive({
    temp <- data_1970
    colnames(temp) <- assets
    temp
    
    #names(my_portfolio) <- assets
    assets4 <<- NULL
    
    for (i in seq_along(my_portfolio())){
      if(my_portfolio()[i]!=0){
        assets4 <-c(assets4, assets[i])
      }
    }
    print(assets4)
    # subset data based on selected assets
    subset_data <- temp[, assets4]
    print(subset_data)
    
    # return xts object
    xts(subset_data, order.by = index(temp))
    
  })
  
  dataset5 <- reactive({
    # assets1 <- input$assets5
    
    # dat_raw <- popSwissr::convert_currencies(symbols=assets1)
    # dat <- timeSeries::returns(dat_raw)
    # #colnames(dat) <- input$assets5
    # print("is(dat)")
    # print(is(dat))
    temp <- data_1970
    colnames(temp) <- assets
    temp
    
    # subset data based on selected assets
    subset_data <- temp[, input$assets5]
    
    # return xts object
    xts(subset_data, order.by = index(temp))
    
    
  })
  
  dataset6 <- reactive({
    temp <- data_1970
    
    assets1 <- input$assets5
    assets1 <- c(assets1, "USDCHF=X")
    # print("assets5")
    # print(assets1)
    # 
    dat_raw <- popSwissr::convert_currencies(symbols=assets1)
    dat_raw <- dat_raw[, !colnames(dat_raw) %in% "USDCHF.X.Adjusted"]
    # print("dat_raw:")
    # print(is(dat_raw))
    # print(dat_raw)
    dat <- timeSeries::returns(dat_raw)
    # print("dat:")
    # print(is(dat))
    
    colnames(dat) <- c(input$assets5)
    # print("colnames(dat)")
    # print(colnames(dat))
    dat <- na.omit(dat)
    # print(dat)
    return(dat)
  })
  
  dataset7 <- reactive({
    temp <- data_1970
    
    assets1 <- input$assets_rendite
    assets1 <- c(assets1, "USDCHF=X")
    # print("assets5")
    # print(assets1)
    # 
    dat_raw <- popSwissr::convert_currencies(symbols=assets1)
    dat_raw <- dat_raw[, !colnames(dat_raw) %in% "USDCHF.X.Adjusted"]
    # print("dat_raw:")
    # print(is(dat_raw))
    # print(dat_raw)
    dat <- timeSeries::returns(dat_raw)
    print("dat:")
    print(is(dat))
    
    colnames(dat) <- c(input$assets_rendite)
    # print("colnames(dat)")
    # print(colnames(dat))
    dat <- na.omit(dat)
    print(dat)
    return(dat)
  })
  
  dataset8 <- reactive({
    temp <- data_1970
    assets1 <- assets3
    assets1 <- c(assets1, "USDCHF=X")
    # print("assets5")
    # print(assets1)
    # 
    dat_raw <- popSwissr::convert_currencies(symbols=assets1)
    dat_raw <- dat_raw[, !colnames(dat_raw) %in% "USDCHF.X.Adjusted"]
    # print("dat_raw:")
    # print(is(dat_raw))
    # print(dat_raw)
    dat <- timeSeries::returns(dat_raw)
    
    colnames(dat) <- assets3
    # print("colnames(dat)")
    # print(colnames(dat))
    dat <- na.omit(dat)
    return(dat)
  })
  
  #changes in individual investment
  indiv_change <- reactive({
    amounts <- c(input$smi2,input$sp5002, input$gold2, input$bitcoin2, input$ch_gov_bonds2, input$us_gov_bonds2)
    return(amounts)
  })
  
  

    

  
  output$smi_plot <- renderPlot({
    
    coulours <- brewer.pal(8, "Dark2")
    plot.xts(dataset2(), bg="transparent", col=coulours, col.lab="gold2", labels.col="navyblue", cex.axis=1.3 , lwd=3)
    #addLegend("topleft", lty=1, lwd=2)
    
   
  }, bg="transparent"  )
  
  #reactive(print(my_portfolio()))
  
  #Individuelles Investment funktion
  indiv <- function(assets, amounts, p_year=260){
    Sigma <- cov(assets)
    r <- apply(X=assets*p_year, MARGIN=2, FUN=mean)
    weights <- amounts/sum(amounts)
    r_pf <- t(weights) %*% r
    v_pf <- sqrt(t(weights) %*% Sigma %*% weights)*sqrt(p_year)
    return(c(weights, r_pf, v_pf))
  }
  
  # Rendite Maximieren Funktion v_opt
  v_opt <- function(assets, v_pf, shorting, p_year=260){
    
    lb <- NA
    ub <- NA
    # import library
    library(nloptr)
    
    # yearly returns, volatility and covariance
    r <- apply(X=assets*p_year, MARGIN=2, FUN=mean)
    print("v_opt1")
    # compute covariance matrix
    Sigma <- p_year*cov(assets)

    # number of assets
    n <- ncol(Sigma)
    
    print("v_opt2")
    
    # Objective function
    eval_f <- function(x, Sigma, r, v_pf){
      return(t(x) %*% r)
    }
    print("v_opt3")
    
    # Constraint
    eval_g_eq <- function(x, Sigma, r, v_pf){
      constr <- c(sum(x) - 1,
                  sqrt(t(x) %*% Sigma %*% x) - v_pf)
      return(constr)
    }
    print("v_opt4")
    
    # lower and upper bounds, with distinction whether shorting is allowed or not
    if(shorting == TRUE){
      lb <- rep(-3, n)
      ub <- rep(3, n)
    }else{
      lb <- rep(0, n)
      ub <- rep(1, n)
    }
    
    print("v_opt5")
    
    # Initial weights
    x0 <- rep(1/n, n)
    
    # set optimization options.
    opts <- list("algorithm"="NLOPT_GN_ISRES",
                 "xtol_rel"=1.0e-15,
                 "maxeval"=160000,
                 "local_opts"=list("algorithm"="NLOPT_LD_MMA", 
                                   "xtol_rel"=1.0e-15),
                 "print_level"=0)
    
    # optimization
    res <- nloptr (x0=x0,
                   eval_f=eval_f,
                   lb=lb,
                   ub=ub,
                   eval_g_eq=eval_g_eq,
                   opts=opts, 
                   Sigma=Sigma,
                   r=r,
                   v_pf=v_pf)
    
    # results
    weights_scal <- res$solution
    pf_return <- as.vector(abs(t(weights_scal) %*% r))
    pf_vola <- v_pf
    
    # return
    return(c(weights_scal, abs(pf_return), pf_vola))
  }
  
  #Risiko Minimieren Funktion r_opt
  r_opt <- function(assets, r_pf, shorting=TRUE, p_year=260){
    # import library
    library(nloptr)
    
    # yearly returns, volatility and covariance
    r <- apply(X=assets*p_year, MARGIN=2, FUN=mean)
    # compute covariance matrix
    Sigma <- cov(assets)
    # number of assets
    n <- ncol(Sigma)
    
    # minimize risk
    # objective function
    eval_f <- function(x, Sigma, r, r_pf){
      # !important!: x = weights
      return(sqrt(t(x) %*% Sigma %*% x))
    }
    
    # lower and upper bounds, with distinction whether shorting is allowed or not
    if(shorting == TRUE){
      lb <- rep(-10, n)
      ub <- rep(10, n)
    }else{
      lb <- rep(0, n)
      ub <- rep(1, n)
    }
    
    # Constraints
    eval_g_eq <- function(x, Sigma, r, r_pf){
      # !important!: x = weights
      constr <- c(sum(x) - 1,
                  x %*% r - r_pf)
      return(constr)
    }
    
    # Initial weights
    x0 <- rep(1/n, n)
    
    # set optimization options
    opts <- list("algorithm"="NLOPT_GN_ISRES",
                 "xtol_rel"=1.0e-15,
                 "maxeval"=160000,
                 "local_opts"=list("algorithm"="NLOPT_LD_MMA",
                                   "xtol_rel"=1.0e-15 ),
                 "print_level"=0)
    
    # optimization
    res <- nloptr(x0=x0,
                  eval_f = eval_f,
                  lb = lb,
                  ub = ub,
                  # eval_g_ineq = eval_g_ineq,
                  eval_g_eq = eval_g_eq,
                  opts = opts, 
                  Sigma = Sigma,
                  r = r,
                  r_pf = r_pf
    )
    
    
    # results
    weights_scal <- res$solution
    pf_return <- r_pf
    pf_vola <- as.vector(sqrt(t(weights_scal) %*% Sigma %*% weights_scal)*sqrt(p_year))
    
    # return
    return(c(weights_scal, abs(pf_return), pf_vola))
  }
  
  
 
  
  #MVP Calculation 
mvp <- function(y){
  N=dim(y)[1]
  #print(N)
  mittel=t(y)%*%rep(1/N,N)*365
  #print(mittel)
  Sigma=cov(y,y) 
  #print(Sigma)
  MVP1=solve(Sigma)%*%rep(1,ncol(y)) 
  MVP=MVP1/sum(MVP1)
  MVP<<-MVP[,1]
  #print(MVP)
  mvpreturn=t(MVP)%*%mittel
  mvpvola=sqrt(t(MVP)%*%(Sigma%*%MVP))*sqrt(365)
  print("mvpvola")
  print(is(mvpvola))
  #returns vector with mvp, return, and volatility
  return( round(c(MVP, mvpreturn, mvpvola),3))
}

#TP Calculation
tp <- function(assets, rf=0.01, p_year=260){ 
  # library 
  library(expm) 
  print("assets in TP")
  print(assets)
  # yearly returns, volatility and covariance 
  yearly_return <- apply(X=assets*p_year, MARGIN=2, FUN=mean) 
  print("yearly returns")
  print(yearly_return)
  
  yearly_volatility <- apply(X=assets*sqrt(p_year), MARGIN=2, FUN=sd) 
  Sigma <- cov(assets); Sigma_inv <- solve(Sigma) 
  print("Sigma correct") 
  print(Sigma)
  # compute weights 
  weights <- Sigma_inv %*% (yearly_return - rf*rep(1, dim(Sigma)[1])) 
  weights_scal <- weights/sum(weights); weights_scal <- as.vector(weights_scal) # ; names(weights_scal) <- colnames(assets) 
  print("weights correct") 
  # compute return and volatility 
  return_TP <- t(weights_scal) %*% yearly_return; return_TP <- as.vector(return_TP) #; names(return_TP) <- "Return Portfolio" 
  volatility_TP <- sqrt(t(weights_scal) %*% Sigma %*% weights_scal)*sqrt(260); volatility_TP <- as.vector(volatility_TP) #; names(volatility_TP) <- "Volatility Portfolio" 
  print("return and vola correct") 
  # return 
  print("return:")
  print(round(c(weights_scal, abs(return_TP), volatility_TP), 3))
  return(round(c(weights_scal, abs(return_TP), volatility_TP), 3)) 
}



  
  #MVP
  output$portfolio_results <- renderText({
    y=dataset3()
    # y=data_1970[]
     y = colDiffs(y)/y[-1,]  #NOCH TEILEN DURCH startwert
    #print(y)
    #y=y[,-2]
    
    mvp_kurs <- mvp(y)
    print(t(mvp_kurs))
    
    
  })
  
  donut <- function(df_donut){
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
      size=7, 
      color="gold3"  ) +
      guides(fill = guide_legend(title = "Anlage", title.position = "top"))+
      annotate("text", x = 0, y = 0, size = 20, color="navyblue", label = paste(abbreviate2(sum(df_donut$value)), "CHF"))+
      theme_void()
  }
  
  
  #create Donut Plot of portfolio
  output$donut_index <- renderPlot({
    #create data frame
    df_donut <- data.frame(value=c(input$smi, input$gold, input$bitcoin, input$ch_gov_bonds, input$us_gov_bonds, input$sp500, input$usd_chf),
                           Anlage=c("SMI", "Gold", "Bitcoin", "Schweizer Staatsanleihen", "US Staatsanleihen", "S&P500", "USD/CHF"))
    
    donut(df_donut)
  }, bg="transparent")
  
  rendite_matrix <- function(x){
    x = colDiffs(x)/x[-1,]
  }
  
  
  output$donut_mvp <- renderPlot({
    #create data frame
    #y <- mvp(rendite_matrix(dataset3()))
    y <- popSwissr::mvp_opt( assets=dataset3(), shorting = input$shorting_mvp, p_year=260)
    print("MVP is")
    print(is(y))
    
    y <- y[1:(length(y)-2)]
    df <- data.frame(value=y*input$mvp_amount,
                           Anlage=rename_assets(colnames(dataset3())))

    # donut(df_donut)
    ggplot(data = df, aes(x = Anlage, y = value, fill = Anlage)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "YlGnBu") +
      geom_text(aes(label = df$value), vjust = -0.5) +
      theme(panel.background = element_rect(fill = "transparent"), # set the background to transparent
            panel.grid.major = element_blank(), # remove the major grid lines
            panel.grid.minor = element_blank(), # remove the minor grid lines
            plot.background = element_rect(fill = NA, color = NA), # set the plot background to white
            axis.line = element_line(color = "black"), # set the axis lines to black
            axis.text = element_text(color = "black"), # set the axis text to black
            axis.title = element_text(color = "black")) # set the axis title to black
  }, bg="transparent")
  
  
  #Create boxplot for TP
  output$donut_tp <- renderPlot({
    y <- tp(dataset6())
        y2 <- y[1:(length(y)-2)]

    df <- data.frame(value=y2*input$tp_amount,
                     Anlage=rename_assets(colnames(dataset6())))
    
    # donut(df_donut)
    ggplot(data = df, aes(x = Anlage, y = value, fill = Anlage)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "YlGnBu") +
      geom_text(aes(label = df$value), vjust = -0.5) +
      theme(panel.background = element_rect(fill = "transparent"), # set the background to transparent
            panel.grid.major = element_blank(), # remove the major grid lines
            panel.grid.minor = element_blank(), # remove the minor grid lines
            plot.background = element_rect(fill = NA, color = NA), # set the plot background to white
            axis.line = element_line(color = "black"), # set the axis lines to black
            axis.text = element_text(color = "black"), # set the axis text to black
            axis.title = element_text(color = "black")) # set the axis title to black
  }, bg="transparent")
  
  #Create boxplot for Rendite Maximieren
  output$boxplot_rendite <- renderPlot({
    print(input$slider_rendite)
    print(input$shorting_rendite)
    y <- v_opt(assets=dataset7(), v_pf=input$slider_rendite, shorting=input$shorting_rendite, p_year=260)
    print("here0")
    y2 <- y[1:(length(y)-2)]
    print("here1")
    
    df <- data.frame(value=y2*input$rendite_amount,
                     Anlage=rename_assets(colnames(dataset7())))
    print("here2")
    
    # donut(df_donut)
    ggplot(data = df, aes(x = Anlage, y = value, fill = Anlage)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "YlGnBu") +
      geom_text(aes(label = df$value), vjust = -0.5) +
      theme(panel.background = element_rect(fill = "transparent"), # set the background to transparent
            panel.grid.major = element_blank(), # remove the major grid lines
            panel.grid.minor = element_blank(), # remove the minor grid lines
            plot.background = element_rect(fill = NA, color = NA), # set the plot background to white
            axis.line = element_line(color = "black"), # set the axis lines to black
            axis.text = element_text(color = "black"), # set the axis text to black
            axis.title = element_text(color = "black")) # set the axis title to black
  }, bg="transparent")
  
  #Create boxplot for Risiko Minimieren
  output$boxplot_risiko <- renderPlot({
    print(input$slider_rendite)
    print(input$shorting_rendite)
    y <- r_opt(assets=dataset7(), r_pf=input$slider_risiko, shorting=input$shorting_risiko, p_year=260)
    print("here0")
    y2 <- y[1:(length(y)-2)]
    print("here1")
    
    df <- data.frame(value=y2*input$rendite_amount,
                     Anlage=rename_assets(colnames(dataset7())))
    print("here2")
    
    # donut(df_donut)
    ggplot(data = df, aes(x = Anlage, y = value, fill = Anlage)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "YlGnBu") +
      geom_text(aes(label = df$value), vjust = -0.5) +
      theme(panel.background = element_rect(fill = "transparent"), # set the background to transparent
            panel.grid.major = element_blank(), # remove the major grid lines
            panel.grid.minor = element_blank(), # remove the minor grid lines
            plot.background = element_rect(fill = NA, color = NA), # set the plot background to white
            axis.line = element_line(color = "black"), # set the axis lines to black
            axis.text = element_text(color = "black"), # set the axis text to black
            axis.title = element_text(color = "black")) # set the axis title to black
  }, bg="transparent")
  
  #Individuelles Investment Boxplot
  output$boxplot_indiv <- renderPlot({
    print("indiv")
    amount_indiv <- indiv_change()
    print(indiv_change())
    y <- indiv(dataset8(), amount_indiv)
    print(y)
    print("indiv2")
    y2 <- y[1:(length(y)-2)]
    print("here1")
    
    df <- data.frame(value=y2*sum(amount_indiv),
                     Anlage=rename_assets(colnames(dataset8())))
    print("here2")
    
    # donut(df_donut)
    ggplot(data = df, aes(x = Anlage, y = value, fill = Anlage)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "YlGnBu") +
      geom_text(aes(label = df$value), vjust = -0.5) +
      theme(panel.background = element_rect(fill = "transparent"), # set the background to transparent
            panel.grid.major = element_blank(), # remove the major grid lines
            panel.grid.minor = element_blank(), # remove the minor grid lines
            plot.background = element_rect(fill = NA, color = NA), # set the plot background to white
            axis.line = element_line(color = "black"), # set the axis lines to black
            axis.text = element_text(color = "black"), # set the axis text to black
            axis.title = element_text(color = "black")) # set the axis title to black
  }, bg="transparent")
  
  #MVP Matrix Mein Portfolio
  output$donut_mvp_my_portfolio <- renderPlot({
    #create data frame
    y <- mvp(rendite_matrix(dataset4()))
    
    
    y <- y[1:(length(y)-2)]
    summe_portfolio <<- sum(my_portfolio())
    df_donut <- data.frame(value=y*summe_portfolio,
                           Anlage=rename_assets(colnames(dataset4())))
    
    donut(df_donut)
  }, bg="transparent")
  
  observeEvent(input$new_investment, {
    updateNavbarPage(session, "navbar", selected="Investment")
  })
  
  get_valueBox_input <- function(){
    
  }
  
  #boxplot of MVP in my_portfolio-page
  output$boxplot_mvp_my_portfolio <- renderPlot({
    y <- mvp(rendite_matrix(dataset4()))
    
    y <- y[1:(length(y)-2)]
    summe_portfolio <<- sum(my_portfolio())
    df <- data.frame(value=y*summe_portfolio,
                           Anlage=rename_assets(colnames(dataset4())))
    
    ggplot(data = df, aes(x = Anlage, y = value, fill = Anlage)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "YlGnBu") +
      geom_text(aes(label = df$value), vjust = -0.5) +
      theme(panel.background = element_rect(fill = "transparent"), # set the background to transparent
            panel.border = element_blank(),
            panel.grid.major = element_blank(), # remove the major grid lines
            panel.grid.minor = element_blank(), # remove the minor grid lines
            plot.background = element_rect(fill = NA, color = NA), # set the plot background to white
            #legend.background = element_blank(),
            axis.line = element_line(color = "black"), # set the axis lines to black
            axis.text = element_text(color = "black"), # set the axis text to black
            axis.title = element_text(color = "black")) # set the axis title to black
  }, bg="transparent")
  
  #boxplot of MVP in my_portfolio-page
  output$boxplot_mvp_my_portfolio <- renderPlot({
    y <- mvp(rendite_matrix(dataset4()))
    
    y <- y[1:(length(y)-2)]
    summe_portfolio <<- sum(my_portfolio())
    df <- data.frame(value=y*summe_portfolio,
                           Anlage=rename_assets(colnames(dataset4())))
    
    ggplot(data = df, aes(x = Anlage, y = value, fill = Anlage)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "YlGnBu") +
      geom_text(aes(label = df$value), vjust = -0.5) +
      theme(panel.background = element_rect(fill = "transparent"), # set the background to transparent
            panel.border = element_blank(),
            panel.grid.major = element_blank(), # remove the major grid lines
            panel.grid.minor = element_blank(), # remove the minor grid lines
            plot.background = element_rect(fill = NA, color = NA), # set the plot background to white
            #legend.background = element_blank(),
            axis.line = element_line(color = "black"), # set the axis lines to black
            axis.text = element_text(color = "black"), # set the axis text to black
            axis.title = element_text(color = "black")) # set the axis title to black
  }, bg="transparent")
  
  #Tabelle MVP
  output$MVP_OUTPUT <- renderTable({
    anteil <- mvp(dataset3())[]
    anteil <- anteil[1:(length(anteil)-2)]
    a <- rbind(rename_assets(colnames(dataset3()))[],paste(percent(anteil)), anteil*summe_portfolio)
    colnames(a)<-a[1,]
    a<-a[-1, ]
    a
     })
  
  #Tabelle TP
  output$TP_OUTPUT <- renderTable({
    anteil <- tp(dataset6())[]
    anteil <- anteil[1:(length(anteil)-2)]
    betrag <- input$tp_amount
    a <- rbind(rename_assets(colnames(dataset6()))[],paste(percent(anteil)), anteil*betrag)
    colnames(a)<-a[1,]
    a<-a[-1, ]
    a
  })
  
  #Box mit Rendite für MVP
  output$mvp_renditeBox <- renderValueBox({
    y <- mvp(dataset3())
    y <- y[length(y)-1]
    valueBox(
      
      paste(y*100, "%"), "Rendite", icon = icon("resize-vertical", lib = "glyphicon"),
      color = "aqua", width=6
    )
  })
  
  #Box mit Risiko für MVP
  output$mvp_risikoBox <- renderValueBox({
    y <- mvp(dataset3())
    y <- y[length(y)]
    
    valueBox(
      paste(y*100, "%"), "Risiko", icon = icon("warning-sign", lib = "glyphicon"),
      color = "aqua", width = 6
    )
  })
  
  #Box mit Rendite für TP
  output$tp_renditeBox <- renderValueBox({
    y <- tp(dataset6())
    y <- y[length(y)-1]
    valueBox(
      
      paste(y*100, "%"), "Rendite", icon = icon("resize-vertical", lib = "glyphicon"),
      color = "aqua", width=6
    )
  })
  
  #Box mit Risiko für TP
  output$tp_risikoBox <- renderValueBox({
    y <- tp(dataset6())
    y <- y[length(y)]
    
    valueBox(
      paste(y*100, "%"), "Risiko", icon = icon("warning-sign", lib = "glyphicon"),
      color = "aqua", width = 6
    )
  })
  
  
  
  
  # Get current location
  current_location <- reactive({
    # Use geosphere package to get current location
    lat <- 0
    lng <- 0
    if (!is.na(Sys.getenv("MAPBOX_TOKEN"))) {
      location <- geosphere::geocode("current location")
      lat <- location$lat
      lng <- location$lon
    }
    return(c(lat, lng))
  })
  
  # Render map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addTiles() %>%
      addMarkers(lng = 8.72923, lat = 47.49732, popup = "Swisscon Hauptsitz") %>%
      #addMarkers(lng = current_location()[2], lat = current_location()[1], popup = "Aktueller Standort") %>%
      #setView(current_location(), zoom = 10)
      setView(lng = 8.72923, lat = 47.49732, zoom = 13)
  })
  
  
  #Box mit Rendite für INDIViduelles
  output$indiv_renditeBox <- renderValueBox({
    y <- indiv(dataset8(), indiv_change())
    y <- y[length(y)-1]
    valueBox(
      
      paste(y*100, "%"), "Rendite", icon = icon("resize-vertical", lib = "glyphicon"),
      color = "aqua", width=6
    )
  })
  
  #Box mit Risiko für Individuelles
  output$indiv_risikoBox <- renderValueBox({
    y <- indiv(dataset8(), indiv_change())
    y <- y[length(y)]
    
    valueBox(
      paste(y*100, "%"), "Risiko", icon = icon("warning-sign", lib = "glyphicon"),
      color = "aqua", width = 6
    )
  })
  
  #Tabelle Individuelles
  output$INDIV_OUTPUT <- renderTable({
    anteil <- indiv(dataset8(), indiv_change())[]
    anteil <- anteil[1:(length(anteil)-2)]
    betrag <- sum(indiv_change())
    a <- rbind(rename_assets(colnames(dataset8()))[],paste(percent(anteil)), anteil*betrag)
    colnames(a)<-a[1,]
    a<-a[-1, ]
    a
  })
  
}
enableBookmarking(store = "url")
shinyApp(ui, server)



