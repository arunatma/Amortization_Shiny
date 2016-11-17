library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("Loan Amortization"),
    
    sidebarPanel
    (
        helpText("This application helps to calculate the EPI - Equated Periodic Instalments"),
        br(),
        
        textInput("principal", "Principal (Rs)", "100000"),
        textInput("rate", "Rate per Annum (%)", "8"),
        textInput("period", "Loan Period (years)", "10"),
        dateInput("cur_date", "Current Date",  language = "en", format = "dd/mm/yyyy"),                 
        dateInput("start_date", "Repayment Start Date",  language = "en", format = "dd/mm/yyyy"),
        selectInput(
            "cfreq", "Compounding Frequency",
                c(Continuous = "conti",
                 Daily = "day",
                 Weekly = "week",
                 Monthly = "month",
                 Quarterly = "quart",
                 Annual = "year")),
        selectInput(
            "pfreq", "Payment Frequency",
                c(Daily = "day",
                 Weekly = "week",
                 Monthly = "month",
                 Quarterly = "quart",
                 Annual = "year"), selected = "year")
    ),
    
    mainPanel(
        tabsetPanel(
            tabPanel("Equated Periodic Instalment", verbatimTextOutput("emival"))
        )
    )
))
    
    
                 