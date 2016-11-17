library(shiny)
library(ggplot2)
library(plotly)
library(data.table)

npv <- function(cur_date, epi_val, pmt_dates, rate, cfreq){
    net_present_value <- 0
    days_diff <- as.integer(pmt_dates - cur_date)
    for(i in days_diff){
        disc_amt <- epi_val / ((1 + rate/cfreq) ** (i/(365.25/cfreq)))
        net_present_value <- net_present_value + disc_amt
    }
    return(net_present_value)
}

npvc <- function(cur_date, epi_val, pmt_dates, rate, cfreq){
    net_present_value <- 0
    days_diff <- as.integer(pmt_dates - cur_date)
    for(i in days_diff){
        disc_amt <- epi_val / exp(rate * i/365.25)
        net_present_value <- net_present_value + disc_amt
    }
    return(net_present_value)
}

getInterest <- function(principal, rate, days, cfreq){
    if(cfreq == 0){
        interest <- principal * (exp(rate * days/365.25) - 1)
    } else {
        interest <- principal * (((1 + rate/cfreq) ** (days/(365.25/cfreq)))- 1)
    }
    return(interest)
}

compoundingFreq <- function(freqStr){
    return(switch(freqStr,
        conti = 0,
        day = 365.25,
        week = 365/7,
        month = 12,
        quart = 4,
        year = 1,
        1))
}

paymentFreq <- function(freqStr){
    return(switch(freqStr,
        day = "1 day",
        week = "1 week",
        month = "1 month",
        quart = "3 months",
        year = "1 year",
        "1 year"))
}   

npvfn <- function(freqStr){
    return(switch(freqStr, conti = npvc, npv))
}

getAmortTable <- function(principal, rate, period, cur_date, start_date, 
    pmt_freq, comp_freq){
    prin <- as.double(principal)
    rate <- as.double(rate) / 100
    period <- as.double(period)
    cur_date <- as.Date(cur_date, format = "%d/%m/%Y")
    cfreq <- compoundingFreq(comp_freq)
    
    start_date <- as.Date(start_date, format = "%d/%m/%Y")
    end_date <- start_date + period * 365.25 - 1
    pmt_dates <- seq(start_date, end_date, by=paymentFreq(pmt_freq))
 
    min_epi <- 0
    max_epi <- prin

    min_npv_val <- npvfn(comp_freq)(cur_date, min_epi, pmt_dates, 
        rate, cfreq)
    max_npv_val <- npvfn(comp_freq)(cur_date, max_epi, pmt_dates, 
        rate, cfreq)
    
    while(abs(min_npv_val - prin) > 0.001){
        mid_epi <- (min_epi + max_epi)/2

        leftdiff <- abs(prin - min_npv_val)
        rightdiff <- abs(max_npv_val - prin)
        
        if(leftdiff < rightdiff){
            max_epi <- mid_epi
            max_npv_val <- npvfn(comp_freq)(cur_date, max_epi, pmt_dates, rate, 
                cfreq)
        } else {
            min_epi <- mid_epi
            min_npv_val <- npvfn(comp_freq)(cur_date, min_epi, pmt_dates, rate, 
                cfreq)
        }
    }
    pmt <- mid_epi
    
    allDates <- c(cur_date, pmt_dates)
    interestDays <- as.numeric(diff(allDates))
    balance <- prin
    pmtSchedule <- data.frame()
    for(i in c(1:length(interestDays))){
        days <- interestDays[i]
        interest <- getInterest(balance, rate, days, cfreq)
        paidPrincipal <- pmt - interest
        balance <- balance - paidPrincipal
        pmtSchedule <- rbind(pmtSchedule, c(interest, paidPrincipal, balance))
    }
    paymentDf <- data.frame(Date = pmt_dates, 
        Instalment = rep(pmt, length(pmt_dates)))
        
    paymentDf <- cbind(paymentDf, pmtSchedule)
    names(paymentDf) <- c("Date", "Instalment", "Interest", "Principal", "Balance")
    return(paymentDf)
}

amortTable <- data.table()
shinyServer(function(input, output){
    amortTable <- reactive({
        getAmortTable(input$principal, input$rate, input$period, 
            input$cur_date, input$start_date, input$pfreq, input$cfreq)
    })
    
    output$emival <- renderPrint({
        amortTable()
    })
    
    output$amortPlot <- renderPlotly({
        amortDt <- data.table(amortTable())
        if(input$showBalance){
            graphTable <- amortDt
        } else {
            graphTable <- amortDt[, !c("Balance"), with=FALSE]
        }
        
        amortTableLong <- melt(graphTable, id="Date")  
        gg <- ggplot(data=amortTableLong, aes(x=Date, y=value, colour=variable)) + geom_line()
        p <- ggplotly(gg)    
    })
})

# getAmortTable(100000, 8, 10, "17/11/2016", "17/11/2016", "month", "month")
