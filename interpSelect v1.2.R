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
       "rmarkdown", "tm", "rhandsontable", "taRifx", "DT", "pdftools", "lmerTest", "grid", "gridExtra", "htmlwidgets", "dygraphs", "plotly", "stringr", "tibble", "zoo", "htmlTable", "ggalt", "pracma", "peakPick")

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

# sampleData <- read.csv(files[1])
# files <- list.files(path =  here("silo-master", "output", "cleanData"), pattern = "csv$", full.names = TRUE)

# Define UI ---------------------------------------------------------------

ui <- fluidPage(title = "IH Interpolation",
  sidebarLayout(
    sidebarPanel(
      selectInput("fileID", "Choose File", choices = idNms),
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
                    column(4, uiOutput("cvParams")),
                    column(4, uiOutput("respParams"))
                    ),
                    br(),
                    fluidRow(
                      column(1,
                             numericInput(inputId = "cuts", label = strong("Cycle Length"),
                                          value = 60)
                      )
                    )
                    ),
                    tabPanel(value = "tab3", "Interpolated Plots", 
                             fluidRow(
                                 column(2,
                                        selectInput(inputId = "plot1Var", label = strong("Variable"),
                                                    choices = NULL)
                                 ),
                               column(2,
                                      numericInput(inputId = "minBout", label = strong("First Bout"),
                                                   value = 1)
                                      ),
                               column(2,
                                      numericInput(inputId = "maxBout", label = "Last Bout",
                                                   value = 10)
                                      )
                               ),
                             fluidRow(
                               column(12,
                                      plotlyOutput("interpPlot")
                               )
                               ),
                             fluidRow(
                               column(2,
                                      checkboxInput(inputId = "meanTrend", "Show Mean")
                                      )
                               ),
                             fluidRow(
                               column(2,
                                      actionButton(inputId = "clearReset", "Reset Cleared Bouts")
                                      ),
                               column(3,
                                      actionButton('saveClean', 'Export Clean Data')),
                               column(3, 
                                      downloadButton("report", "Download Report")
                                      )
                             )
                               )
                             )
        )
        )
      )
      )
)



# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  # Get unique study and participant ids from folder
  allFls <- list.files(path = paste0(here::here(),"/interpolationApp/appData"), pattern = ".csv$")
  idNms <- sapply(allFls, function(x) strsplit(x, "\\_|\\."))
  idNms <- sapply(idNms, function(x) x[2])
  idNms <- unique(idNms)

  # Read breath file based on file choice
bData <- reactive({
    breathfileName <- paste0(here::here(),"/interpolationApp/appData/","breath_",input$fileID,".csv")
    df <- read.csv(paste(breathfileName), header = TRUE)
    if(!is.na(input$fileStart)){df <- df[df$Time >= input$fileStart,]}
    if(!is.na(input$fileEnd)){df <- df[df$Time <= input$fileEnd,]}
    if(!is.na(input$protoStart)){
      df$RelTime <- df$Time - input$protoStart
      df$RelTime <-  df$RelTime - df$RelTime[which.min(abs(df$RelTime  - 0))]
    }
    df
  })

# Read beat file based on file choice
cvData <- reactive({
  cvfileName <- paste0(here::here(),"/interpolationApp/appData/","beat_",input$fileID,".csv")
  beatdf <- read.csv(paste(cvfileName), header = TRUE)
  if(!is.na(input$fileStart)){beatdf <- beatdf[beatdf$Time >= input$fileStart,]}
  if(!is.na(input$fileEnd)){beatdf <- beatdf[beatdf$Time <= input$fileEnd,]}
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



# Interpolated Plot ------------------------------------------------------
# choose variable to plot

observeEvent(input$tabs, if(input$tabs == "tab3"){
  
observe({
  updateSelectInput(session,"plot1Var",choices=c(input$cvParamsIn,input$respParamsIn),
                      selected = NULL)
})


# Interpolate Function ----------------------------------------------------

interpData <- reactive({

  bData1 <- bData()
  cvData1 <- cvData()

  iData <-  cbind(bData1, cvData1[-c(grep("Time", colnames(cvData1)),
                           grep("id", colnames(cvData1)),
                           grep("bout", colnames(cvData1)),
                           grep("cuts", colnames(cvData1)))])

  iData$cuts <- as.character(cut(iData$Time, breaks = seq(min(iData$Time), max(iData$Time)+input$cuts,
                                                          by = input$cuts), include.lowest = FALSE))
  iData$cuts <- strsplit(iData$cuts, ",")

  for (i in 2:length(iData$cuts)) {
    iData$cuts[[i]] <- as.numeric(substr(iData$cuts[[i]][[2]], 1, nchar(iData$cuts[[i]][[2]])-1))
  }
  iData$cuts <- as.numeric(iData$cuts)
  iData$bTime <- rep(1:as.numeric(input$cuts))
  iData$bout <- round(iData$cuts/as.numeric(input$cuts),0)
  iData$bout <- as.factor(iData$bout)

  iData

})


# Interpolation plot ------------------------------------------------------

rxVals <- reactiveValues(plotData = NULL)

observeEvent(input$fileID,{ 
  interpData <- interpData()
  rxVals$plotData <-  data.frame(matrix(1,nrow = isolate(nrow(interpData())),
                                          ncol = isolate(ncol(interpData())), byrow = FALSE))
  rxVals$boutOut <-  data.frame(matrix(0,nrow = isolate(nrow(interpData())),
                                        ncol = isolate(ncol(interpData())), byrow = FALSE))
  colnames(rxVals$plotData) <- colnames(interpData())
  colnames(rxVals$boutOut) <- colnames(interpData())
  
  })

observeEvent(c(input$minBout, input$maxBout), {
  interpData <- interpData()
  if(!is.na(input$minBout) & !is.na(input$maxBout)){
    boutRange <- input$minBout:input$maxBout
  }else{
    boutRange <- 1:10
  }
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  rxVals$plotData[which(interpData$bout %!in% boutRange),] <- 0
  rxVals$plotData[which(interpData$bout %in% boutRange),] <- 1
  })

interpClean <- reactive({
  interpData <- interpData()
  

  d <- as.numeric(event_data("plotly_click")[4])
  # if(length(d)>0){rxVals$plotData <- rxVals$plotData[-which(rxVals$plotData$bout == d),]}
  boutOut <- unique(interpData[which(interpData[,input$plot1Var] == d),'bout'])
  rxVals$boutOut[which(interpData$bout == boutOut),] <- 1
  xx <- (rxVals$boutOut*-1) + rxVals$plotData
  interpData[which(xx$bout ==1),]
})


meanData <- reactive({
    interpData <- interpClean()
    meanData <- interpData %>%
      group_by(bTime) %>%
      summarise_at(vars(input$plot1Var), .funs = c("mean" = mean, "sem" = std.error), na.rm = TRUE)
    
    meanData
  })
  
  
output$interpPlot <- renderPlotly({
    interpData <- interpClean()

    p3 <- ggplot(interpData, aes_string(x = "bTime", y = input$plot1Var)) +
      geom_line(aes(colour = bout, group = "bout")) +
      xlab("Time (s)") +
      theme_economist(base_size = 14) +
      theme(legend.position = "none") 
    
    if(input$meanTrend){p3 <- p3 + stat_smooth(method = loess, size = 2)}
        # geom_line(data = meanData, aes_string(x = "bTime", y = input$plot1Var), inherit.aes = FALSE)
    
    ggplotly(p3)
  })


# Save Clean Data ---------------------------------------------------------
# outFile <- reactive({
#   fileID <- fileID()
#   useFileID <- subset(fileIndex, subject == input$subjectId & cond1 == input$cond1)
#   if(noCond() >= 2){useFileID <- subset(useFileID, cond2 == input$cond2)}
#   if(noCond() == 3){useFileID <- subset(useFileID, cond3 == input$cond3)}
#   useFileID <- apply(useFileID[,-c(1, ncol(useFileID))],1, paste, collapse = "_")
#   useFileID[1]
# })

observeEvent(input$saveClean,{ #### called from UI
  fd <- input$fileID
  fileName <- fd
  fileName <- paste(fileName, "-clean.csv", sep = "")
  dframe <- interpClean()
  cvColRange <- which(!is.na(match(colnames(dframe), input$cvParamsIn)))
  timeCol <- which(colnames(dframe) == "Time")
  bTimeCol <- which(colnames(dframe) == "bTime")
  idCol <- which(colnames(dframe) == "id")
  boutCol <- which(colnames(dframe) == "bout")
  cvColRange <- c(idCol, timeCol, bTimeCol, boutCol, cvColRange)
  respColRange <- which(!is.na(match(colnames(dframe), input$respParamsIn)))
  dframe <- dframe[,c(cvColRange, respColRange)]
  write.csv(dframe, paste(here::here("interpolationApp", "cleanData"),"/", fileName, sep = ""), row.names = FALSE)
})

output$report <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = "report.html",
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy(here::here("interpolationApp/interpReports/report.Rmd"), tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params <- list(interpData = interpClean(),
                   fileID = input$fileID
                   )
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
                      
    )
  }
)
})

}

shinyApp(ui = ui, server = server)
