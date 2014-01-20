library(shiny)
library("TTR")

##Oh hai!!
add_stuff = function(ost,aggtype,timeline,boundaries){
  cat("\nadding stuff\n")
  ost$Year <- substr(ost$dttm_utc,0,4)
  ost$Month <- substr(ost$dttm_utc,6,7)
  ost$Day <-substr(ost$dttm_utc,9,10)
  ost$Hour <- substr(ost$dttm_utc,12,13)
  ost$Minute <- substr(ost$dttm_utc,15,16)
  ost$Second <- substr(ost$dttm_utc,18,19)
  ost$CuMinute <- as.numeric(ost$Minute) + 60 * as.numeric(ost$Hour)
  ost$Weekday <- weekdays(as.Date(ISOdatetime(ost$Year,ost$Month,ost$Day,ost$Hour,ost$Minute,ost$Second,tz="utc")))
  ost_len = nrow(ost)
  ost_min = as.integer(ost_len * min(boundaries) / 100)
  ost_max = as.integer(ost_len * max(boundaries) / 100)
  ost <- ost[ost_min:ost_max,]
  
  if (timeline == "Hour of Day"){
    if (aggtype=="All")
      agg_plot <<- aggregate(ost$value,by=list(ost$Hour), FUN=mean)
    else if (aggtype=="Saturday")
      agg_plot <<- aggregate(ost[ost$Weekday=="Saturday",]$value,by=list(ost[ost$Weekday=="Saturday",]$Hour), FUN=mean)
    else if (aggtype=="Sunday")
      agg_plot <<- aggregate(ost[ost$Weekday=="Sunday",]$value,by=list(ost[ost$Weekday=="Sunday",]$Hour), FUN=mean)
    else if (aggtype=="Business Days")
      agg_plot <<- aggregate(ost[ost$Weekday!="Sunday" & ost$Weekday!="Saturday",]$value,by=list(ost[ost$Weekday!="Sunday" & ost$Weekday!="Saturday",]$Hour),FUN=mean)
    return(plot(agg_plot))
  }
  else if (timeline == "Day of Month"){
    if (aggtype=="All")
      agg_plot <<- aggregate(ost$value,by=list(ost$Day), FUN=mean)
    else if (aggtype=="Saturday")
      agg_plot <<- aggregate(ost[ost$Weekday=="Saturday",]$value,by=list(ost[ost$Weekday=="Saturday",]$Day), FUN=mean)
    else if (aggtype=="Sunday")
      agg_plot <<- aggregate(ost[ost$Weekday=="Sunday",]$value,by=list(ost[ost$Weekday=="Sunday",]$Day), FUN=mean)
    else if (aggtype=="Business Days")
      agg_plot <<- aggregate(ost[ost$Weekday!="Sunday" & ost$Weekday!="Saturday",]$value,by=list(ost[ost$Weekday!="Sunday" & ost$Weekday!="Saturday",]$Day),FUN=mean)
    return(plot(agg_plot))
  }
  else if (timeline == "None"){
    if (aggtype=="All")
      agg_plot <<- ost$value
    else if (aggtype=="Saturday")
      agg_plot <<- ost[ost$Weekday=="Saturday",]$value
    else if (aggtype=="Sunday")
      agg_plot <<- ost[ost$Weekday=="Sunday",]$value
    else if (aggtype=="Business Days")
      agg_plot <<- ost[ost$Weekday!="Sunday" & ost$Weekday!="Saturday",]$value
    return(plot(agg_plot))
  }
} 

shinyServer(function(input,output){

  output$summary1 = renderPrint({
    dataset = ost()
    summary(dataset$value)
  })
  
  output$Caption1 = renderText({
    paste("Filename:",input$filename,sep=" ")
  })
  output$Caption2 = renderText({
    paste("Day(s): ",input$weekday,sep=" ")
  })
  output$Caption3 = renderText({
    paste("Aggregation:",input$aggtype,sep=" ")
  })
  output$Caption4 = renderText({
    paste("Time Range: ",paste(input$date_range,collapse="% - "),"%",sep="")
  })
  
  ost <- reactive({
    data = read.csv(paste("/home/rstudio_user/enernoc/csv/",input$filename,sep=""))
    
  })
  
  
  output$agg_plot = renderPlot({
    data <- ost()
    add_stuff(data,input$weekday,input$aggtype,input$date_range)
  })    
  
  output$agg_plot = renderPlot({
    
  })
  
#   browser()
#   output$caption = renderText({
#     formulaText()
#   })
  
  
  
})