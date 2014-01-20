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
                min=0,max=100,step=5, value = c(20,45))
    ,
    checkboxInput("abs_rel","Set static min and max demand on Y-axis",FALSE)
    ,
    checkboxInput("trend","Decompose Timeseries",FALSE)
    ,
    checkboxInput("complex","Fit more complex models",FALSE)
    
    
  ),

  mainPanel(
     h3(textOutput("Caption1")) ,
     
    
    h4(textOutput("Exp1")),
    plotOutput("agg_plot"),
    h4(textOutput("Exp2")),
    verbatimTextOutput("summary1"),
    h4(textOutput("Exp3")),
    plotOutput("ts_decomp"),
    h4(textOutput("Exp4")),
    plotOutput("ts_minus_seasonal")#,
#     plotOutput("decomp_acf"),
#     plotOutput("decomp_pacf"),
#     plotOutput("auto_arima"),
#     plotOutput("arima_acf"),
#     plotOutput("arima_pacf")
    
  )
))

