#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    ##React Values storage for Financial Instruments Selections by users
    React.FinInst <- reactiveValues()

    ## Record all player selections reactive on match and build dataframe of player selections
    observeEvent(input$FinInstrument, {
        
        ##Store the instrument selections
        React.FinInst$Instruments <- input$FinInstrument
        
        if (input$FinInstrument == "Stocks") {
            React.FinInst$TickerChoices <- Stocks.Tickers
        } else if (input$FinInstrument == "ETF") {
            React.FinInst$TickerChoices <- ETF.Tickers
        } else if (input$FinInstrument == "Bonds") {
            React.FinInst$TickerChoices <- Bonds.Tickers
        }
        ###################################################################################
        ## update Picker input choices of Home and Away players dropdown choices based on selected Match reactively
        updatePickerInput(session = session, inputId = 'FinTicker', 
                          choices = React.FinInst$TickerChoices)
    })

    ## Populate Reactive values storage with users Tickers selections inputs
    observeEvent(input$FinTicker, {
        
        #Populate the reactive values storage
        React.FinInst$TickerSelects <- input$FinTicker

    })

    ## Data Representation (Reactive on ShowData button)
    TickerData <- eventReactive(input$ShowData, {
        
        ## Read in Ticker data from yahoo via quantmod package
        DataTicker.All <- sapply(React.FinInst$TickerSelects, FUN = function(x) {
            #Pull in data
            Data <- getSymbols(Symbols = paste0(x, ".JK"), src = "yahoo", from = input$DatesRange[1], to = input$DatesRange[2], auto.assign = F)
            Data
            #Wrangle Data
            
            
        }, simplify = F)
        browser()
        indf <- getSymbols(Symbols = "INDF.JK", src = "yahoo", from = Sys.Date() - 1953, to = Sys.Date(), auto.assign = F)
        bca <- getSymbols(Symbols = "BBCA.JK", src = "yahoo", from = Sys.Date() - 1953, to = Sys.Date(), auto.assign = F)
        antm <- getSymbols(Symbols = "ANTM.JK", src = "yahoo", from = Sys.Date() - 1953, to = Sys.Date(), auto.assign = F)
        untr <- getSymbols(Symbols = "UNTR.JK", src = "yahoo", from = Sys.Date() - 1953, to = Sys.Date(), auto.assign = F)
        
    })
    
    FinancialData <- observeEvent(TickerData(), {
        #If users wants data table
        # if (input$Format == "Table") {
        #     browser()
        # } else if (input$Format == "Chart") {
        #     browser()
        # }
        browser()
        
    })

})
