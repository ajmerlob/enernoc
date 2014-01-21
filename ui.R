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
    checkboxInput("trend","Decompose Time Series",FALSE)
    ,
    checkboxInput("complex","Difference Model",FALSE)
    ,
    checkboxInput("arima","Fit ARIMA",FALSE)
    ,
    checkboxInput("easter","Easter Egg",TRUE)
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
    plotOutput("ts_minus_seasonal"),
    h4(textOutput("Exp5")),
    plotOutput("decomp_acf"),
    plotOutput("decomp_pacf"),

    h4(textOutput("Exp6")),
    plotOutput("diff_plot"),
    plotOutput("diff_acf"),
    plotOutput("diff_pacf"),
    
    h4(textOutput("Exp7")),
    plotOutput("arima_text"),
    h4(textOutput("Exp8")),
    
    h3(textOutput("Caption2")) ,
    h4(textOutput("Exp9"))
  )
))

