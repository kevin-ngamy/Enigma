ui <- fluidPage(
    # theme = bs_theme(version = 3, bootswatch = "journal"),
    
    # Application title
    titlePanel(strong("Enigma Trading")),
    br(),
    
    # Code for sidebar layout
    sidebarPanel(
        conditionalPanel(condition = "input.tabselected==1",
                         align = "left",
                         tags$h3("Financial Instruments"),
                         pickerInput(inputId = "FinInstrument", 
                                     label = "Select Financial Instruments", 
                                     choices = c("Stocks", "ETF", "Bonds"), 
                                     selected = "Stocks"), 
                         ## Reactive Dropdown Selections of Financial tricker corresponds to Instruments selections
                         pickerInput(inputId = "FinTicker", 
                                     label = "Select Ticker", 
                                     choices = "",
                                     options = list(`live-search` = TRUE, `actions-box` = TRUE),
                                     multiple = T),
                         ## Period Slider
                         dateRangeInput(inputId = "DatesRange",
                                     label = "Dates:",
                                     start = "2018-01-01",
                                     end = paste(Sys.Date())), 
                         ## Input for table/chart representation
                         pickerInput(inputId = "Format", 
                                     label = "Data Representation", 
                                     choices = c("Table", "Chart"), 
                                     multiple = F), 
                         ## Action Button to represent the data
                         br(),
                         br(actionBttn(inputId = "ShowData", block = T,
                                       label = "Show Data",
                                       style = "unite", 
                                       color = "primary",
                                       icon = icon("table"), size = "lg"))
                         
                         ), width = 2), 
    mainPanel(
        tabsetPanel(type = "tabs", id = "tabselected", selected = 1,
                    tabPanel("Data Representation",
                             DT::dataTableOutput("Data1", width = "100%"),
                             DT::dataTableOutput("Data2", width = "100%"),
                             DT::dataTableOutput("Data3", width = "100%"),
                             DT::dataTableOutput("Data4", width = "100%"),
                             DT::dataTableOutput("Data5", width = "100%"),
                             value = 1))))

