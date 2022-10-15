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

        #Now Show 5 of the data divided into 5 chunks of table
        for (i in 1:length(DataTicker.All)) {
            # browser()
            output$Data1 <- renderDataTable({
                DT::datatable(
                    DataTicker.All[[1]], 
                    options = list(
                        dom = 't',
                        deferRender = TRUE,
                        scroller = TRUE,
                        scrollY = 750,
                        scrollX = 750,
                        pagewidth = 500, 
                        dom = 'lBfrtip', paging = FALSE)
                )
            })
            
            output$Data2 <- renderDataTable({
                DT::datatable(
                    DataTicker.All[[2]], 
                    options = list(
                        dom = 't',
                        deferRender = TRUE,
                        scroller = TRUE,
                        scrollY = 750,
                        scrollX = 750,
                        pagewidth = 500,
                        dom = 'lBfrtip', paging = FALSE
                    )
                )
            })
            
            output$Data3 <- renderDataTable({
                DT::datatable(
                    DataTicker.All[[3]], 
                    options = list(
                        dom = 't',
                        deferRender = TRUE,
                        scroller = TRUE,
                        scrollY = 750,
                        scrollX = 750,
                        pagewidth = 500,
                        dom = 'lBfrtip', paging = FALSE
                    )
                )
            })
            
            output$Data4 <- renderDataTable({
                DT::datatable(
                    DataTicker.All[[4]], 
                    options = list(
                        dom = 't',
                        deferRender = TRUE,
                        scroller = TRUE,
                        scrollY = 750,
                        scrollX = 750,
                        pagewidth = 500, 
                        dom = 'lBfrtip', paging = FALSE
                    )
                )
            })
            
            output$Data5 <- renderDataTable({
                DT::datatable(
                    DataTicker.All[[5]], 
                    options = list(
                        dom = 't',
                        deferRender = TRUE,
                        scroller = TRUE,
                        scrollY = 750,
                        scrollX = 750,
                        pagewidth = 500, 
                        dom = 'lBfrtip', paging = FALSE
                    )
                )
            })
            
        }
        
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
