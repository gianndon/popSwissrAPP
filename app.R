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
library(shinydashboard)
library(nloptr)
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
assets <- c("^SSMI", "USDCHF=X", "^GSPC", "GC=F", "BTC-USD", "CSBGC0.SW", "^TNX")
                #c("SMI", "USD / CHF", "S&P500", " ", "Bitcoin USD", " ", " ", " "))
assets1 <- c("SMI"="^SSMI", "USD/CHF"="USDCHF=X", "S&P500"="^GSPC", "Gold"="GC=F", "Bitcoin USD"="BTC-USD", "CH Staatsanleihen"="CSBGC0.SW", "US Staatsanleihen"="^TNX")

title <- tags$a(tags$img(src="Swisscon_Logo2.png", height="45px", id="logo"))

#data up to 1970
data_1970 <- popSwissr::convert_currencies(assets, Sys.Date()-30*365, Sys.Date())




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
                                   column(3,),
                                   br(),
                                   br(),
                                   fluidRow(),
                                   column(6, 
                                          h1("MVP"),
                                          plotOutput("donut_mvp_my_portfolio", height= "45vh")),
                                   column(6, h1("TP")),
                                   width ="100vh")),
                            
                          tabPanel("Kursübersicht",
                                   dataTableOutput("test"),
                                   #fluidRow(),
                                   titlePanel("Kursübersicht für SMI, S&P500, Gold,..."),
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
                          tabPanel("Rendite Maximieren / Risiko Minimieren"),
                          tabPanel("Minimum Varianz Portfolio",
                                   h2("Minimum Varianz Portfolio"),
                                   column(9, checkboxGroupInput("assets3", "Select Assets:", choices = assets1, 
                                                                selected = assets1,
                                                                inline=TRUE,
                                                                #multiple = TRUE,
                                   ), id="kursuebersicht_style"),
                                   column(3,numericInput("mvp_amount", "Amount [CHF]", value = 20000)),
                                   br(),
                                   column(9, plotOutput("donut_mvp", height= "65vh")),
                                   column(3,verbatimTextOutput("portfolio_results")),
                                   fluidRow(
                                     tableOutput("MVP_OUTPUT")
                                   ),
                                 
                                   
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
    ch_gov_bonds <<- input$us_gov_bonds
    gold <- input$gold
    bitcoin <- input$bitcoin
    us_gov_bonds <- input$us_gov_bonds
    sp500 <- input$sp500
    usd_chf <- input$usd-chf-devisen
    
  })
  
  #c("SMI"="^SSMI", "USD/CHF"="USDCHF=X", "S&P500"="^GSPC", "Gold"="GC=F", "Bitcoin USD"="BTC-USD", "CH Staatsanleihen"="CSBGC0.SW", "US Staatsanleihen"="^TNX")
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
    colnames(temp) <- assets
    temp
    
    # subset data based on selected assets
    subset_data <- temp[, input$assets3]
    
    # return xts object
    xts(subset_data, order.by = index(temp))
    
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
    
    # return xts object
    xts(subset_data, order.by = index(temp))
    
  })
  
  output$smi_plot <- renderPlot({
    
    coulours <- brewer.pal(8, "Dark2")
    plot.xts(dataset2(), bg="transparent", col=coulours, col.lab="gold2", labels.col="navyblue", cex.axis=1.3 , lwd=3)
    addLegend("topleft", lty=1, lwd=2)
    
   
  }, bg="transparent"  )
  
  #reactive(print(my_portfolio()))
 
  
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
  #returns vector with mvp, return, and volatility
  return( round(c(MVP, mvpreturn, mvpvola),3))
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
    y <- mvp(rendite_matrix(dataset3()))
    
    y <- y[1:(length(y)-2)]
    df_donut <- data.frame(value=y*input$mvp_amount,
                           Anlage=colnames(dataset3()))

    donut(df_donut)
  }, bg="transparent")
  
  #MVP Matrix Mein Portfolio
  output$donut_mvp_my_portfolio <- renderPlot({
    #create data frame
    y <- mvp(rendite_matrix(dataset4()))
    
    y <- y[1:(length(y)-2)]
    summe_portfolio <<- sum(my_portfolio())
    df_donut <- data.frame(value=y*summe_portfolio,
                           Anlage=colnames(dataset4()))
    
    donut(df_donut)
  }, bg="transparent")
  
  observeEvent(input$new_investment, {
    updateNavbarPage(session, "navbar", selected="Investment")
  })
  
  get_valueBox_input <- function(){
    
  }
  
  output$MVP_OUTPUT <- renderTable({
    anteil <- mvp(rendite_matrix(dataset3()))[]
    a <- rbind((anteil*100),colnames(dataset3())[], anteil*summe_portfolio)
    a
    # valueBox(
    #   paste0(round(mvp(rendite_matrix(dataset3()))[2]*100,2), "%"),
    #   subtitle=colnames(dataset3())[2],
    #   icon=NULL,
    #   color="blue",
    #   width=4,
    #   href=NULL
    # )
    
  })
  
  
}
enableBookmarking(store = "url")
shinyApp(ui, server)
