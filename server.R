library(shiny)
library(TTR)

##Oh hai!!
append_datetime = function(ost){
  ost$Year <- substr(ost$dttm_utc,0,4)
  ost$Month <- substr(ost$dttm_utc,6,7)
  ost$Day <-substr(ost$dttm_utc,9,10)
  ost$Hour <- substr(ost$dttm_utc,12,13)
  ost$Minute <- substr(ost$dttm_utc,15,16)
  ost$Second <- substr(ost$dttm_utc,18,19)
  ost$CuMinute <- as.numeric(ost$Minute) + 60 * as.numeric(ost$Hour)
  ost$Weekday <- weekdays(as.Date(ISOdatetime(ost$Year,ost$Month,ost$Day,ost$Hour,ost$Minute,ost$Second,tz="utc")))
  return(ost)
} 

get_slice <- function(ost,boundaries){
  ost_len = nrow(ost)
  ost_min = as.integer(ost_len * min(boundaries) / 100)
  ost_max = as.integer(ost_len * max(boundaries) / 100)
  return(ost[ost_min:ost_max,])
}

get_dynamic_plot <- function(ost2,aggtype,timeline,boundaries,abs_rel){
  ost_min = min(ost2$value)
  ost_max = max(ost2$value)
  
  ost = get_slice(ost2,boundaries)
  
  if (timeline == "Hour of Day"){
    if (aggtype=="All")
      agg_plot <- aggregate(ost$value,by=list(ost$Hour), FUN=mean)
    else if (aggtype=="Saturday")
      agg_plot <- aggregate(ost[ost$Weekday=="Saturday",]$value,by=list(ost[ost$Weekday=="Saturday",]$Hour), FUN=mean)
    else if (aggtype=="Sunday")
      agg_plot <- aggregate(ost[ost$Weekday=="Sunday",]$value,by=list(ost[ost$Weekday=="Sunday",]$Hour), FUN=mean)
    else if (aggtype=="Business Days")
      agg_plot <- aggregate(ost[ost$Weekday!="Sunday" & ost$Weekday!="Saturday",]$value,by=list(ost[ost$Weekday!="Sunday" & ost$Weekday!="Saturday",]$Hour),FUN=mean)

    if (aggtype=="All")
      time_plot <- aggregate(ost$timestamp,by=list(ost$Hour), FUN=mean)
    else if (aggtype=="Saturday")
      time_plot <- aggregate(ost[ost$Weekday=="Saturday",]$timestamp,by=list(ost[ost$Weekday=="Saturday",]$Hour), FUN=mean)
    else if (aggtype=="Sunday")
      time_plot <- aggregate(ost[ost$Weekday=="Sunday",]$timestamp,by=list(ost[ost$Weekday=="Sunday",]$Hour), FUN=mean)
    else if (aggtype=="Business Days")
      time_plot <- aggregate(ost[ost$Weekday!="Sunday" & ost$Weekday!="Saturday",]$timestamp,by=list(ost[ost$Weekday!="Sunday" & ost$Weekday!="Saturday",]$Hour),FUN=mean)

  }
  else if (timeline == "Day of Month"){
    if (aggtype=="All")
      agg_plot <- aggregate(ost$value,by=list(ost$Day), FUN=mean)
    else if (aggtype=="Saturday")
      agg_plot <- aggregate(ost[ost$Weekday=="Saturday",]$value,by=list(ost[ost$Weekday=="Saturday",]$Day), FUN=mean)
    else if (aggtype=="Sunday")
      agg_plot <- aggregate(ost[ost$Weekday=="Sunday",]$value,by=list(ost[ost$Weekday=="Sunday",]$Day), FUN=mean)
    else if (aggtype=="Business Days")
      agg_plot <- aggregate(ost[ost$Weekday!="Sunday" & ost$Weekday!="Saturday",]$value,by=list(ost[ost$Weekday!="Sunday" & ost$Weekday!="Saturday",]$Day),FUN=mean)
    cat("Return 2\n")
    
    if (aggtype=="All")
      time_plot <- aggregate(ost$timestamp,by=list(ost$Day), FUN=mean)
    else if (aggtype=="Saturday")
      time_plot <- aggregate(ost[ost$Weekday=="Saturday",]$timestamp,by=list(ost[ost$Weekday=="Saturday",]$Day), FUN=mean)
    else if (aggtype=="Sunday")
      time_plot <- aggregate(ost[ost$Weekday=="Sunday",]$timestamp,by=list(ost[ost$Weekday=="Sunday",]$Day), FUN=mean)
    else if (aggtype=="Business Days")
      time_plot <- aggregate(ost[ost$Weekday!="Sunday" & ost$Weekday!="Saturday",]$timestamp,by=list(ost[ost$Weekday!="Sunday" & ost$Weekday!="Saturday",]$Day),FUN=mean)
    
  }
  else if (timeline == "None"){
    if (aggtype=="All")
      agg_plot <- ost$value
    else if (aggtype=="Saturday")
      agg_plot <- ost[ost$Weekday=="Saturday",]$value
    else if (aggtype=="Sunday")
      agg_plot <- ost[ost$Weekday=="Sunday",]$value
    else if (aggtype=="Business Days")
      agg_plot <- ost[ost$Weekday!="Sunday" & ost$Weekday!="Saturday",]$value
    cat("Return 3\n")
    if (aggtype=="All")
      time_plot <- ost$timestamp
    else if (aggtype=="Saturday")
      time_plot <- ost[ost$Weekday=="Saturday",]$timestamp
    else if (aggtype=="Sunday")
      time_plot <- ost[ost$Weekday=="Sunday",]$timestamp
    else if (aggtype=="Business Days")
      time_plot <- ost[ost$Weekday!="Sunday" & ost$Weekday!="Saturday",]$timestamp
  }
  ost_sub ="Set static Y-axis range using checkbox on left | Plot Uses Day of Week Filtering, Aggregation Type, and Date Range %"
  if (timeline=="None"){
    if(abs_rel){
      return(plot(time_plot,agg_plot,sub=ost_sub,type="p",xlab="Timestamp",ylab="Total Demand",main="Demand Over Time",ylim=c(ost_min,ost_max),xlim=c(min(time_plot),max(time_plot))))
    } else {
      return(plot(time_plot,agg_plot,sub=ost_sub,type="p",xlab="Timestamp",ylab="Total Demand",main="Demand Over Time",xlim=c(min(time_plot),max(time_plot))))
    }
  } else{
    if(abs_rel){
      return(plot(agg_plot,sub=ost_sub,type="p",xlab=timeline,ylab="Average Total Demand",main="Demand Over Time",ylim=c(ost_min,ost_max)))
    } else {
      return(plot(agg_plot,sub=ost_sub,type="p",xlab=timeline,ylab="Average Total Demand",main="Demand Over Time"))
    }
  }
}

show_trend <- function(ost2,boundaries){
  ost = get_slice(ost2,boundaries)
  ost_ts = ts(ost$value,frequency=2016)
  ost_dec = decompose(ost_ts)
  return(ost_dec)
}

shinyServer(function(input,output){
  output$Caption1 = renderText({
    paste(paste("Filename:",input$filename,sep=" "), "|",paste("Day(s): ",input$weekday,sep=" "),"|",paste("Aggregation:",input$aggtype,sep=" "),"|",paste("Date Range: ",paste(input$date_range,collapse="% - "),"%",sep=""),sep=" ")
  })
  
  ost <- reactive({
    data = append_datetime(read.csv(paste("/home/rstudio_user/enernoc/csv/",input$filename,sep="")))
  })
  
  output$Exp1 = renderText({
    "As you can see in the graph below, energy demand forms a time series, usually with pronounced cycles based on time of day and day of week.  Use the 'Day of Week Filtering' and 'Aggregation Type' selectors on the left to drill down on the different patterns.  You can use the Date Range % slider to visualize different parts of the dataset.  To force the Y-axis minimum and maximum values to remain the same while you change the filtering and aggregation, use the checkbox on the left."
  })
  
  output$agg_plot = renderPlot({
    data <- ost()
    get_dynamic_plot(data,input$weekday,input$aggtype,input$date_range,input$abs_rel)
  })    
  
  output$Exp2 = renderText({
    "For the time slice shown above, the following summary statistics apply:"
  })

  output$summary1 = renderPrint({
    dataset = get_slice(ost(),input$date_range)
    cat("Summary Statistics for Demand (Within Current Date Range)\n")
    summary(dataset$value)
  })
  
  output$Exp3 = renderText({
    "Unfortunately, for a time series with seasonality (expressed in the hourly and daily cycles mentioned before), these summary statistics are not particularly helpful in thinking about the data.  One first step we might take is to make an adjustment to separate the seasonality from the rest of the data (and try to visualize the seasonality).  If the fluctuations in demand values seem to be fairly consistent in magnitude over time (and the seasonality seems to be fairly consistent in magnitude both over time and relative to the level of overall demand), we can consider using an additive model to decompose the time series into components.  The following graph shows the time series decomposed into ‘trend,’ ‘seasonal,’ and ‘random’ components, which represent the changes in the average level of demand, the repeated fluctuations observed at different points in time, and fluctuations that are not predicted by either of the first two components, but were observed at a specific time, respectively.  [[Click 'Decompose Time Series' on left sidebar to see graphs]]"
  })
  
  decom = reactive({
    show_trend(ost(),input$date_range)
  }) 
  
  
  output$ts_decomp = renderPlot({
    if (input$trend){
      plot(decom())
    }  else {}
  })
  
  output$Exp4 = renderText({
    "To the extent that an additive model captures the time series, the trend component above shows how the overall level of demand changes over the course of time.  We can try to improve our understanding of how demand changes over time by using more sophisticated models.  Let's run some diagnostics on the seasonally adjusted time series (pictured below)."
  })

  
  ts_seasonally_adjusted_t = reactive({
    if (input$trend){
      ts(decom()$x-decom()$seasonal,frequency=2016)
    }  else {}
  })
  
  ts_seasonally_adjusted_c = reactive({
    if (input$complex){
      ts(decom()$x-decom()$seasonal,frequency=2016)
    }  else {}
  })
  
  ts_seasonally_adjusted_a = reactive({
    if (input$arima){
      ts(decom()$x-decom()$seasonal,frequency=2016)
    }  else {}
  })
  
  
  
  output$ts_minus_seasonal = renderPlot({
    if (input$trend){
      plot(ts_seasonally_adjusted_t(),main="Seasonally Adjusted Demand",xlab="Observations Since Start of Date Range",ylab="Seasonally Adjusted Demand")
    }  else {}
  })
  
  output$Exp5 = renderText({
    "The ACF and PACF can help us identify modeling changes that can improve the fit.  These are pictured below.  The ACF shows the degree to which values of a time series are correlated with the values that came before.  Since any correlation between a value and it’s immediately preceding value would tend to propagate backwards through earlier and earlier values, the Partial ACF is useful to find the autocorrelations not accounted for by this propagation."
  })
  
  output$decomp_acf = renderPlot({
    if (input$trend){
      plot(acf(ts_seasonally_adjusted_t(),na.action=na.contiguous,lag.max=30),
           main="Autocorrelation of Seasonally Adjusted Demand",
           sub="Lags are expressed as a fraction of a week (2016 observations)"
           )
    } else {}
  })
  output$decomp_pacf = renderPlot({
    if (input$trend){
      plot(pacf(ts_seasonally_adjusted_t(),na.action=na.contiguous,lag.max=30),
           main="Partial Autocorrelation of Seasonally Adjusted Demand",
           sub="Lags are expressed as a fraction of a week (2016 observations)"
           )
    } else {}
  })
  
  output$Exp6 = renderText({
    "When the ACF shows large autocorrelations tapering off over many lags, one approach is the difference the time series.  Differencing adds the previous demand value as a predictor of the current demand value.  Most of the files display this signature, so the differenced values are visualized below. [[Click 'Difference Model' on left sidebar to see graphs]]"
  })
  
  diff_plot <- reactive({
    if (input$complex){
    diff(ts_seasonally_adjusted_c())
    } else {}
  })
  
  output$diff_plot = renderPlot({
    if (input$complex){
      plot(diff_plot(),main='Differenced Seasonally Adjusted Demand',ylab="Differenced Seasonally Adjusted Demand",xlab="Time in Weeks")
    } else {}
  })
  
  output$diff_acf = renderPlot({
    if (input$complex){
      plot(acf(diff_plot(),na.action=na.contiguous,lag.max=30),
           main="Autocorrelation of Differenced Seasonally Adjusted Demand",
           sub="Lags are expressed as a fraction of a week (2016 observations)"
      )
    } else {}
  })
  output$diff_pacf = renderPlot({
    if (input$complex){
      plot(pacf(diff_plot(),na.action=na.contiguous,lag.max=30),
           main="Partial Autocorrelation of Differenced Seasonally Adjusted Demand",
           sub="Lags are expressed as a fraction of a week (2016 observations)"
      )
    } else {}
  })
  
  output$Exp7 = renderText({
    "If the differenced time series looks like white noise, and the associated ACF and PACF do not have any significant values in their signatures, then we have built an adequate model from the time series data, and would want to pursue other predictors.  On the other hand, if the ACF of PACF still show significant values (more lines above the blue dotted line than would be expected by chance), than we can attempt to find a better fit by adding an autoregressive feature or a moving average feature to our model (either in addition to, or instead of the difference term in our model). [[Click ‘Fit ARIMA’ on the left sidebar to fit an ARIMA model]]"
  })
  
  
  arima_model = reactive({
    if (input$arima){
      auto.arima(ts_seasonally_adjusted_a(),stationary=TRUE,seasonal=FALSE,
                 max.d=2,
                 max.p=3,
                 max.q=3
                 )
    } else {}
  })
  
  output$arima_text <- renderPlot({
    if (input$arima){
      cat("starting")
      plot(forecast(arima_model(),h=5),xlab="Time in Weeks",ylab="Demand",col="red")
      par(new=TRUE)
      plot(ts_seasonally_adjusted_a(),xlab="",ylab="",col="black")
      cat("ending")
    } else {}
  })
  
  output$Exp8 = renderText({
    "The ARIMA model above was chosen because it minimized the AIC (Akaike information criterion), a metric that can be used to compare the fit of different ARIMA models (in relative terms, not in absolute terms). In a more sophisticated setting, a custom error metric might be used that incorporated a closer approximation of the cost of the error.  For example, a custom error metric might assign a larger loss when the model underestimated demand versus when the model overestimated demand, because it is more costly to generate additional electricity to meet higher-than-expected demand than it is to reduce electricity output.  The forecasts from the ARIMA model are shown in red alongside the actual demand shown in black."
  })
  
  output$Caption2 = renderText({
    "Easter Egg - Did You Know?"
  })
  
  output$Exp9 = renderText({
    "The Easter holiday is known for wreaking havoc on time series analysis!  Because Easter can fall anywhere from March 22nd to April 25th, holiday-based effects can come in different quarters, months, weeks and days from year to year.  While this electricity demand time series only has one Easter, you can still find an Easter-related anomaly (an Easter Egg, one might say) in the data.  This anomaly shows up on the Friday before, and only in some time series (try 109.csv if you haven’t found one yet). The following plot shows average demand aggregated by hour – typical for all Fridays in the year is plotted in black, and the Friday before Easter (April 6th in 2012) is plotted in red."
  })
  
  output$easter_plot <- renderPlot({
    if(input$easter){
      data = ost()
      agg_plot <- aggregate(data[data$Month=="03" & data$Weekday=="Friday",]$value,by=list(data[data$Month=="03" & data$Weekday=="Friday",]$Hour), FUN=mean)
      plot(agg_plot,type="p",xlab="Hour of Day",ylab="Average Total Demand",main="Demand Over Time")
      
      easter_plot <- aggregate(data[data$Month=="04" & data$Day=="06" & data$Year=="2012",]$value,by=list(data[data$Month=="04" & data$Day=="08" & data$Year=="2012",]$Hour), FUN=mean)
      par(new=TRUE)
      plot(easter_plot,xlab="",ylab="",col="red", axes=FALSE)
      
    } else {}
  })
  
})