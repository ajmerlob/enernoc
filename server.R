library(shiny)
library(TTR)

##Oh hai!!
add_stuff = function(ost2){
  ost = ost2
  #cat("\nadding stuff\n")
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
#   cat("Slice\n")
  ost_len = nrow(ost)
#   cat(ost_len)
#   cat("\n")
  ost_min = as.integer(ost_len * min(boundaries) / 100)
  ost_max = as.integer(ost_len * max(boundaries) / 100)
  return(ost[ost_min:ost_max,])
}

get_slice_and_plot <- function(ost2,aggtype,timeline,boundaries,abs_rel){
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
      return(plot(agg_plot,sub=ost_sub,type="p",xlab=timeline,ylab="Total Demand",main="Demand Over Time",ylim=c(ost_min,ost_max)))
    } else {
      return(plot(agg_plot,sub=ost_sub,type="p",xlab=timeline,ylab="Total Demand",main="Demand Over Time"))
    }
  }
}

show_trend <- function(ost2,boundaries){
  ost = get_slice(ost2,boundaries)
  ost_ts = ts(ost$value,frequency=2016)
  ost_dec = decompose(ost_ts)
  return(ost_dec)
}

get_arima <- function(ost2,boundaries){
  ost = get_slice(ost2,boundaries)
  ost_ts_diff = diff(ts(ost$value,frequency=2016))
  return(ost_ts_diff)
}

shinyServer(function(input,output){
  output$Caption1 = renderText({
    paste(paste("Filename:",input$filename,sep=" "), "|",paste("Day(s): ",input$weekday,sep=" "),"|",paste("Aggregation:",input$aggtype,sep=" "),"|",paste("Date Range: ",paste(input$date_range,collapse="% - "),"%",sep=""),sep=" ")
  })
  
  ost <- reactive({
    data = add_stuff(read.csv(paste("/home/rstudio_user/enernoc/csv/",input$filename,sep="")))
  })
  
  output$Exp1 = renderText({
    "As you can see in the graph below, energy demand forms a time series, usually with pronounced cycles based on time of day and day of week.  Use the 'Day of Week Filtering' and 'Aggregation Type' selectors on the left to drill down on the different patterns.  You can use the Date Range % slider to visualize different parts of the dataset.  To force the Y-axis minimum and maximum values to remain the same while you change the filtering and aggregation, use the checkbox on the left."
  })
  
  output$agg_plot = renderPlot({
    data <- ost()
    get_slice_and_plot(data,input$weekday,input$aggtype,input$date_range,input$abs_rel)
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
    "Unfortunately, for a time series with seasonality (expressed in the hourly and daily cycles mentioned before), these summary statistics are not particularly helpful in thinking about the data.  One first step we might take is to visualize and make an adjustment to separate the seasonality from the rest of the data.  If the fluctuations in demand values seem to be fairly consistent in magnitude over time (and the seasonality seems to be fairly consistent in magnitude both over time and relative to the level of overall demand), we can consider using an additive model to decompose the time series into components.  The following graph shows the time series decomposed into ‘trend,’ ‘seasonal,’ and ‘random’ components, which represent the changes in the average level of demand, the repeated fluctuations observed at different points in time, and fluctuations that are not predicted by either of the first two components, but were observed at a specific time, respectively.  [[Click 'Decompose Timeseries' on left sidebar to see graphs]]"
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
    "Here is a look at the time seasonally adjusted time series:"
  })
  
  output$ts_minus_seasonal = renderPlot({
    if (input$trend){
      plot(decom()$x-decom()$seasonal)
    }  else {}
  })
  
  output$Exp4 = renderText({
    "To the extent that an additive model captures the time series, the trend component above shows how the overall level of demand changes over the course of time.  We can try to improve our understanding of how demand changes over time by using a more sophisticated model."
  })
  
  output$ts_decomp = renderPlot({
    if (input$trend){
      plot(decom())
    }  else {}
  })
  
  
  

  
  
  
  
  
  
  
  

  output$decomp_acf = renderPlot({
    if (input$trend){
      plot(acf(ts(decom()$random,frequency=2016),na.action=na.contiguous,lag.max=30),
           sub="Lags are expressed as a fraction of a week (2016 observations)"
           )
    } else {}
  })
  output$decomp_pacf = renderPlot({
    if (input$trend){
      plot(pacf(ts(decom()$random,frequency=2016),na.action=na.contiguous,lag.max=30),
           sub="Lags are expressed as a fraction of a week (2016 observations)"
           )
    } else {}
  })
  
  diff_plot = reactive({
    ts(get_arima(ost(),input$date_range),frequency=2016)
  })
  
  output$auto_arima = renderPlot({
    if (input$complex){
      plot(diff_plot())
    } else {}
  })
  output$arima_acf = renderPlot({
    if (input$complex){
      plot(acf(diff_plot(),na.action=na.contiguous,lag.max=30),
           sub="Lags are expressed as a fraction of a week (2016 observations)"
      )
    } else {}
  })
  output$arima_pacf = renderPlot({
    if (input$complex){
      plot(pacf(diff_plot(),na.action=na.contiguous,lag.max=30),
           sub="Lags are expressed as a fraction of a week (2016 observations)"
      )
    } else {}
  })
  
  
#   browser()
#   output$caption = renderText({
#     formulaText()
#   })
  
  
  
})