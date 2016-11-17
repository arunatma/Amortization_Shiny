library(shiny)

#Function to calculate NPV value            
npv <- function(cur_date, epi_val, gen_seq, rate, cfreq){
    net_present_value <- 0
    days_diff <- as.integer(gen_seq - cur_date)
    for(i in days_diff){
        disc_amt <- epi_val / ((1 + rate/cfreq) ** (i/(365.25/cfreq)))
        net_present_value <- net_present_value + disc_amt
    }
    return(net_present_value)
}

npvc <- function(cur_date, epi_val, gen_seq, rate, cfreq){
    net_present_value <- 0
    days_diff <- as.integer(gen_seq - cur_date)
    for(i in days_diff){
        disc_amt <- epi_val / exp(rate * i/365.25)
        net_present_value <- net_present_value + disc_amt
    }
    return(net_present_value)
}
    
shinyServer(function(input, output){

    cfreq <- reactive({
        switch(input$cfreq,
            conti = 0,
            day = 365.25,
            week = 365/7,
            month = 12,
            quart = 4,
            year = 1,
            1)
    })
    
    pfreq <- reactive({
        switch(input$pfreq,
            day = "1 day",
            week = "1 week",
            month = "1 month",
            quart = "3 months",
            year = "1 year",
            "1 year")
    })    
    
    npvfn <- reactive({
        switch(input$cfreq,
               conti = npvc,
               npv)
    })
    
    amortTable <- reactive({
        prin <- as.double(input$principal)
        rate <- as.double(input$rate) / 100
        period <- as.double(input$period)
        cur_date <- as.Date(input$cur_date, format = "%d/%m/%Y")
        
        start_date <- as.Date(input$start_date, format = "%d/%m/%Y")
        end_date <- start_date + period * 365.25 - 1
        gen_seq <- seq(start_date, end_date, by=pfreq())
     
        min_epi <- 0
        max_epi <- prin

        min_npv_val <- npvfn()(cur_date, min_epi, gen_seq, rate, cfreq())
        max_npv_val <- npvfn()(cur_date, max_epi, gen_seq, rate, cfreq())
        
        while(abs(min_npv_val - prin) > 0.001){
            mid_epi <- (min_epi + max_epi)/2

            leftdiff <- abs(prin - min_npv_val)
            rightdiff <- abs(max_npv_val - prin)
            
            if(leftdiff < rightdiff){
                max_epi <- mid_epi
                max_npv_val <- npvfn()(cur_date, max_epi, gen_seq, rate, cfreq())
            } else {
                min_epi <- mid_epi
                min_npv_val <- npvfn()(cur_date, min_epi, gen_seq, rate, cfreq())
            }
        }
        data.frame(Date = gen_seq, Instalment = rep(mid_epi, length(gen_seq)))
    })
    
    output$emival <- renderPrint({
        amortTable()
    })    
    
})