library(shiny)


##Oh hai!!
shinyUI(pageWithSidebar(

  headerPanel("EnerNOC Data Visualizations"),

  sidebarPanel(
    selectInput("filename","Filename:",
                choices = list.files('/home/rstudio_user/enernoc/csv/',pattern='.csv'))
    ,
    selectInput("weekday","Day of Week Filtering:",choices=c("All","Saturday","Sunday","Business Days"))
    ,
    selectInput("aggtype","Aggregation Type:",choices=c("None","Day of Month","Hour of Day"))
    ,
    sliderInput("date_range","Date Range %:",
                min=0,max=100, value = c(20,30))
  ),

  mainPanel(
     h3(textOutput("Caption1")) ,
     h3(textOutput("Caption2")) ,
     h3(textOutput("Caption3")) ,
     h3(textOutput("Caption4")) ,
     
    verbatimTextOutput("summary1"),
     
    plotOutput("agg_plot")
    ,
    plotOutput("ts_decomp")
  )
))

