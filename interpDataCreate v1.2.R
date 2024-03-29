#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

y <- c("shiny", "plyr", "dplyr", "reshape", "shinythemes", "ggplot2", "ggthemes", "readr", "shinyFiles",
       "shinyjs", "taRifx", "shinydashboard", "here", "plotrix", "data.table", "gridExtra",
       "rmarkdown", "tm", "rhandsontable", "taRifx", "DT", "pdftools", "lmerTest", "grid", "gridExtra", "htmlwidgets", "dygraphs", "plotly", "peakPick")

for(i in 1:length(y)){
  is_installed <- function(mypkg){is.element(mypkg,
                                             installed.packages()[,1])}
  if(!is_installed(y[i])){ 
    install.packages(y[i],
                     repos="http://lib.stat.cmu.edu/R/CRAN")
  }
  library(y[i], character.only=TRUE,
          quietly=TRUE,verbose=FALSE)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Interpolate Data"),
   
   # Sidebar with a slider input for number of bins 
   fluidPage(title = "Interpolate Data",
             sidebarLayout(
               sidebarPanel(uiOutput('idNms'),
                checkboxInput("trendLine", "Add trend line"),
                 numericInput(inputId = "fileStart", label = strong("Choose File Start"),
                              value = NA),
                 numericInput(inputId = "fileEnd", label = strong("Choose File End"),
                              value = NA),
                 numericInput("protoStart", label = strong("Choose Start of Protocol"),
                              value = NA),
                 width = 2),
               
               mainPanel(
                 fluidRow(
                   column(12,
                          tabsetPanel(id = "tabs", type = "tabs",
                                      tabPanel(value = "tab1","Plots",
                                               fluidRow(
                                                 column(2,
                                                        uiOutput("ventVarsSelect")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(12,
                                                        plotlyOutput("rawBreathPlot")
                                                 )
                                               ),
                                               br(),
                                               br(),
                                               br(),
                                               fluidRow(
                                                 column(2,
                                                        uiOutput("cvVarsSelect")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(12,
                                                        plotlyOutput("rawBeatPlot")
                                                 )
                                               )
                                      ),
                                      tabPanel(value = "tab2", "Select Variables",
                                               br(),
                                               fluidRow(
                                                 column(1,
                                                        numericInput(inputId = "cuts", 
                                                                     label = strong("Cycle Length"),
                                                                     value = 60)
                                                 ),
                                                 column(4, uiOutput("cvParams")),
                                                 column(4, uiOutput("respParams"))
                                               ),
                                               br(),
                                               fluidRow( 
                                                 checkboxInput("interpCheck", "Interpolate Data")
                                               ),
                                               tableOutput("tableCheck")
                                      ),
                                      tabPanel(value = "tab3", "Quality Check",
                                               fluidRow(
                                                 column(2, selectInput(inputId = "plot1Var", label = strong("Variable"),
                                                                 choices = NULL)
                                                     )
                                                 ),
                                               fluidRow(
                                                 column(12,
                                                      plotlyOutput("interpPlot")
                                               )
                                               ),
                                               fluidRow(
                                                 column(2,
                                                        actionButton(inputId = "expData", "Export Data")
                                                        )
                                               )
                                               )
                                               )
                          )
                   )
                 )
               )
             )
   )

   

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Get unique study and participant ids from folder
  output$idNms <- renderUI({
    allFls <- list.files(path = paste0(here::here(),"/rawData"), pattern = ".csv$")
    idNms <- sapply(allFls, function(x) strsplit(x, "\\_|\\."))
    idNms <- sapply(idNms, function(x) x[2])
    idNms <- unique(idNms)
    selectInput("fileID", "Choose File", choices = idNms)
  })
  
  
  # Read breath file based on file choice
  bData <- reactive({
    breathfileName <- paste0(here::here(),"/rawData/","breath_",input$fileID,".csv")
    df <- read.csv(paste(breathfileName), header = TRUE)
    if(!is.na(input$fileStart)){df <- df[which(df$Time > input$fileStart),]}
    if(!is.na(input$fileEnd)){df <- df[which(df$Time < input$fileEnd),]}
    if(!is.na(input$protoStart)){
      df$RelTime <- df$Time - input$protoStart
      df$RelTime <-  df$RelTime - df$RelTime[which.min(abs(df$RelTime  - 0))]
    }
    
    df
  })
  
  # Read beat file based on file choice
  cvData <- reactive({
    cvfileName <- paste0(here::here(),"/rawData/","beat_",input$fileID,".csv")
    beatdf <- read.csv(paste(cvfileName), header = TRUE)
    if(!is.na(input$fileStart)){beatdf <- beatdf[which(beatdf$Time >= input$fileStart),]}
    if(!is.na(input$fileEnd)){beatdf <- beatdf[which(beatdf$Time <= input$fileEnd),]}
    if(!is.na(input$protoStart)){
      beatdf$RelTime <- beatdf$Time - input$protoStart
      beatdf$RelTime <-  beatdf$RelTime - beatdf$RelTime[which.min(abs(beatdf$RelTime  - 0))]
    }
    beatdf
  })
  
  
  # Add variable selection --------------------------------------------------
  
  output$ventVarsSelect <- renderUI({
    req(input$fileID)
    selectInput(inputId = "ventVars", label = strong("Select Ventilatory Variable"), choices = colnames(bData()))
    # selected = input$ventVar)
  })
  
  output$cvVarsSelect <- renderUI({
    req(input$fileID)
    selectInput(inputId = "cvVars", label = strong("Select Cardiovascular Variable"), choices = colnames(cvData()))
    # selected = input$ventVar)
    
  })
  # Create Raw Plots --------------------------------------------------------
  # Resp Var Select
  
  output$rawBreathPlot <- renderPlotly({
    if(!is.na(input$protoStart)){xVar <- "RelTime"} else {xVar <- "Time"}
    p1 <- ggplot(bData(), aes_string(x = xVar, y = input$ventVars))+
      geom_point() +
      theme_economist()
    
    if(input$trendLine){p1 <- p1 + stat_smooth(method = glm, size = 2)}
    p1 <- ggplotly(p1)
    p1})
  
  output$rawBeatPlot <- renderPlotly({
    if(!is.na(input$protoStart)){xVar <- "RelTime"} else {xVar <- "Time"}
    p2 <- ggplot(cvData(), aes_string(x = xVar, y = input$cvVars))+
      geom_point() +
      theme_economist()
    
    if(input$trendLine){p2 <- p2 + stat_smooth(method = glm, size =2)}
    p2 <- ggplotly(p2)
    p2})
  
  
  
  # Choose Variables to Process  ---------------------------------------------------------
  # observeEvent(input$tabs, if(input$tabs == "tab2"){
  output$cvParams <- renderUI({cvData <- cvData()
  selectInput(inputId = "cvParamsIn", label = strong("Select Cardiovascular Variables"),choices = colnames(cvData), multiple=TRUE, selectize=TRUE)
  })
  
  output$respParams <- renderUI({bData <- bData()
  selectInput(inputId = "respParamsIn", label = strong("Select Respiratory Variables"),choices = colnames(bData), multiple=TRUE, selectize=TRUE)
  })
  

  splineInterp <- function(df, timeCol){
    newDf <- df[1:2400,]
    newDf$Time <- approx(x = df[,timeCol], y = df[,3], n = 2400, method = "linear")[[1]]
    for(i in 3:ncol(df)){
      if(all(is.na(df[,i]))){newDf[1:2400,i] <- paste(df[1:2400,i])} else{
        newDf[,i] <- approx(x = df[,timeCol], y = df[,i], n = 2400, method = "linear")[[2]]
      }
    }
    colnames(newDf) <- colnames(df)
    newDf[,"Time"] <- 1:2400
    return(newDf)
  }
  

  # Create Interpolated df --------------------------------------------------
  
  
  cvData1 <- reactive({
    req(cvData())
  cvData1 <- cvData()

  cvData1 <- splineInterp(cvData1, timeCol = "Time")

  cvData1
  })

  bData1 <- reactive({
     req(bData())
     bData1 <- bData()

     bData1 <- splineInterp(bData1, "Time")
  
    bData1
   })
   
interpData <- reactive({
  req(bData1())
  req(cvData1())
  
  bData1 <- bData1()
  cvData1 <- cvData1()
  
  iData <-  cbind(bData1, cvData1[-c(grep("Time", colnames(cvData1)),
                                     grep("id", colnames(cvData1)))])
  iData$cuts <- as.character(cut(iData$Time, breaks = seq(min(iData$Time), max(iData$Time)+input$cuts,
                                                          by = input$cuts), include.lowest = FALSE))
  iData$cuts <- strsplit(iData$cuts, ",")
  
  for (i in 2:length(iData$cuts)) {
    iData$cuts[[i]] <- as.numeric(substr(iData$cuts[[i]][[2]], 1, nchar(iData$cuts[[i]][[2]])-1))
  }
  iData$cuts <- as.numeric(iData$cuts)
  iData$bTime <- rep(1:as.numeric(input$cuts))
  iData$bout <- iData$cuts/as.numeric(input$cuts)
  iData$bout <- round(iData$bout, 0)
  iData$bout <- as.factor(iData$bout)
  
  iData
})


# Tab 3 -------------------------------------------------------------------

observe({
  updateSelectInput(session,"plot1Var",choices=c(input$cvParamsIn,input$respParamsIn),
                    selected = NULL)
})

output$interpPlot <- renderPlotly({
  interpData <- interpData()
  p3 <- ggplot(interpData, aes_string(x = "bTime", y = input$plot1Var, group = "bout")) +
                 geom_line() +
                 theme_dark()
  ggplotly(p3)
              
})

cvDataSave <- reactive({
  cvData1 <- cvData1()
  interpData <- interpData()
  
  cvDfSave <- interpData[,c(colnames(cvData1), "cuts", "bout")]
  cvDfSave
  })

bDataSave <- reactive({
  bData1 <- bData1()
  interpData <- interpData()
  
  bDfSave <- interpData[,c(colnames(bData1), "cuts", "bout")]
  bDfSave
  })

observeEvent(input$expData,{
  cvDataSave <- cvDataSave()
  bDataSave <- bDataSave()
  
  fileName <- input$fileID
  cvfileName <- paste0("beat_", fileName, ".csv")
  bfileName <- paste0("breath_", fileName, ".csv")
  
  write.csv(cvDataSave, paste(here::here("/interpolationApp/appData"),"/", cvfileName, sep = ""), row.names = FALSE)
  write.csv(bDataSave, paste(here::here("/interpolationApp/appData"),"/", bfileName, sep = ""), row.names = FALSE)
})

  
}

# Run the application 
shinyApp(ui = ui, server = server)

