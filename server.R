
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

    #Populate data presentation format
    observeEvent(input$Format, {
        React.FinInst$PresentFormat <- input$Format
    })
    ## Data Representation (Reactive on ShowData button)
    TickerData <- eventReactive(input$ShowData, {
        
        #Define Progress Bar
        YahooScprProg <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(YahooScprProg$close())
        YahooScprProg$set(message = paste0("Scraping Financial Data from Yahoo Finance "), value = 0)
        progincrement <<- 1/length(React.FinInst$TickerSelects)
        
        ## Read in Ticker data from yahoo via quantmod package
        DataTicker.xts <- sapply(React.FinInst$TickerSelects, FUN = function(x) {
            
            YahooScprProg$inc(amount = progincrement, detail = paste0("Reading ", paste0(x, ".JK ", input$FinInstrument, " data")))
            ## Read in raw data from yahoo finance
            ts.data.raw <- getSymbols(Symbols = paste0(x, ".JK"), src = "yahoo", from = input$DatesRange[1], to = input$DatesRange[2], auto.assign = F)
            
            #Wrangle 
            ts.data <- data.frame(Date=index(ts.data.raw), coredata(ts.data.raw))
            ts.data <- ts.data %>%
                arrange(desc(Date))
            ts.data$Ticker <- x
            
            #Wrangle column names
            colnames(ts.data)[grepl("Open", colnames(ts.data))] <- "Open"
            colnames(ts.data)[grepl("High", colnames(ts.data))] <- "High"
            colnames(ts.data)[grepl("Low", colnames(ts.data))] <- "Low"
            colnames(ts.data)[grepl("Close", colnames(ts.data))] <- "Close"
            colnames(ts.data)[grepl("Volume", colnames(ts.data))] <- "Volume"
            colnames(ts.data)[grepl("Adjusted", colnames(ts.data))] <- "Adjusted"
            list(XTS = ts.data.raw, dataframe = ts.data)
            
        }, simplify = F)
        
        #Dataframe format
        DataTicker.All <- map(DataTicker.xts, .f = function(x) {
            x$dataframe
        })
        DataTicker.xts <- map(DataTicker.xts, .f = function(x) {
            x$XTS
        })
        
        DataTicker.All <- data.frame(rbindlist(DataTicker.All))
        DataTicker.All <- subset(DataTicker.All, select = c(Ticker, Date, Open, High, Low, Close, Volume, Adjusted))
        
        #store both data as list
        list(DataTicker = DataTicker.All, XTS = DataTicker.xts)
        
    })
    
    #Render Ticker data as data table output
    output$TickerDataTable <- renderDataTable({
        
        DT::datatable(
            TickerData()[["DataTicker"]], 
            extensions = c("Buttons"),
            options = list(
                # dom = 't',
                # deferRender = TRUE,
                buttons = list('csv'),
                scroller = TRUE,
                scrollY = 750,
                scrollX = 750,
                # pagewidth = 500, 
                paging = FALSE), 
            filter = list(position = "top"), 
            rownames = F)
    })
    
    #Update UI sidebar panel for 2nd tab (visualization tab choices)
    observeEvent(TickerData(), {
        updatePickerInput(session = session, inputId = "Sel.TickerViz", 
                          choices = React.FinInst$TickerSelects)
    })
    
    #Visualization 
    Ticker.Viz <- eventReactive(input$ShowViz, {
        
        DataViz <- TickerData()[["XTS"]]
        #Filter
        TickerDataViz <- DataViz[[input$Sel.TickerViz]]
        TickerDataViz
    })

    ##Render visualization
    output$TickerDataViz <- renderPlot({
        chartSeries(Ticker.Viz(), theme="white") 
        addSMA(n = input$MA1, on = 1, with.col = Cl, overlay = TRUE, col = "brown")
        addSMA(n = input$MA2, on = 1, with.col = Cl, overlay = TRUE, col = "brown")
        addBBands(n = input$BB.MAperiod, sd = input$BB.sdev, maType = "SMA", draw = 'bands', on = -1)
    })
    
    ##  3rd Sub Tab (Statistical Summary)
    #Update ticker choice
    observeEvent(TickerData(), {
        updatePickerInput(session = session, inputId = "FinTicker.FinSummary", 
                          choices = React.FinInst$TickerSelects)
    })
    
    Fin.Stats.Summary <- eventReactive(input$ShowData.FinSummary, {
        
        Data <- TickerData()[["XTS"]] #pull data
        #Take Difference
        Return.Dt <- lapply(Data, function(x) {
            
            #Take the 1 day difference 
            Logdt <- log(x)
            #Impute missval
            Logdt <- na.locf(Logdt, na.rm = T, fromLast = T)
            Lagdt <- diff(Logdt, lag = 1) #lag data
            Lagdt
        })
        
        #Filter Returndt
        Return.Dt <- Return.Dt[c(input$FinTicker.FinSummary)]
        list(Data = Return.Dt, AnalType = input$AnalType.FinSummary)
    })
    
    #summViz
    output$SummaryPlot <- renderPlot({
        #Conditional Viz
        Data <- Fin.Stats.Summary()[["Data"]][[input$FinTicker.FinSummary]]
        print(Data)
        FinalViz <- Data[,grepl("Adjusted", colnames(Data))] 
        
        if (Fin.Stats.Summary()[["AnalType"]] == "Log Return") {
            plot(FinalViz)
        } else if (Fin.Stats.Summary()[["AnalType"]] == "Log Return Distribution") {

            FinalViz <- FinalViz[!is.na(FinalViz)]
            hist(FinalViz, prob = TRUE, breaks = 30, ylim = c(0, 50),
                 main = paste(input$FinTicker.FinSummary, " Log Return Distribution"))
            lines(density(FinalViz), col = "blue", lwd = 3)
            
        } else if (Fin.Stats.Summary()[["AnalType"]] == "Simple Return") {
            FinalViz <- FinalViz[!is.na(FinalViz)]
            hist(exp(FinalViz), prob = TRUE, breaks = 30, ylim = c(0, 50),
                 main = paste(input$FinTicker.FinSummary, " Simple Return Distribution"))
            lines(density(exp(FinalViz)), col = "blue", lwd = 3)
        }
    })
    
    #SummPRint
    output$SummaryTable <- renderTable({
        Data <- Fin.Stats.Summary()[["Data"]][[input$FinTicker.FinSummary]]
        Data <- Data[,colnames(Data)[grepl("Adjusted", colnames(Data))]]
        CurrTicker <- colnames(Data)[grepl("Adjusted", colnames(Data))]
        StatSum <- data.frame(basicStats(Data))
        StatSum <- data.frame(Stats = rownames(StatSum), Vals = StatSum[,CurrTicker])
        #pivot
        StatSum <- StatSum %>% pivot_wider(names_from = Stats, values_from = Vals)
        StatSum
        
        
    })

})
