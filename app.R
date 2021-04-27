library(shiny)
library(jsonlite)
library(dplyr)
library(plotly)
library(data.table)
library(DT)
library(ggforce)
library(shinycssloaders)
#set size limit for results files input
maxMegaBytesSize <-  50
options(shiny.maxRequestSize = maxMegaBytesSize * 1024 ^ 2)


#set size of displayed sample
sampleSize <-  10000

#define colors 
inputColor <- "#29AF7F"
outputColor <- "#482677"

  
setwd("~/dev/RopenMOLE/")  
source("./OMdataRead.R")
source("./evolutionPlot.R")
source("./evolutionMetadataRead.R")
source("./PSEMetadataRead.R")








# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("R OpenMOLE"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file1",
        "Choose OpenMole result file",
        multiple = F,
        placeholder = "No more than 50 MB",
        accept = c("tar.gz",
                   "tgz")
      ),
      checkboxInput(
        "sizeLimiting",
        "subset of 10000 sampled lines",
        value = FALSE,
        width = NULL
      ),
      br(),
      p("bla bla bla bla pookie"),
      br(),
      p("version 0.1"),
      width = 2
    )#sidebarPanel
    ,
    
    mainPanel(
      tabsetPanel(
        tabPanel("MetaData",  uiOutput("metadataDisplay") %>% withSpinner()),
        tabPanel("Input-OutPut Spaces",
                 fluidRow(
                   column(6, plotlyOutput("inputSpace") %>% withSpinner()),
                   column(6, plotlyOutput("outputSpace") %>% withSpinner())
                 )),
        #second tabPanel
        tabPanel(
          "Objectives over time",
          plotOutput("objective_over_gen", height = "250px") %>% withSpinner(),
          checkboxInput("make_obj_gen_Boxplot", "draw boxplot by generation", value = FALSE)
        ),
        #third tabpanel (objectives over time)
        tabPanel("Back and Forth",
                 plotlyOutput("pingpong") %>% withSpinner()),
        tabPanel(
          "Full result data",
          DT::dataTableOutput("fulldataDisplay") %>% withSpinner()
        )
      )
    )#mainPanel
  )#sidebarlayout
  )#ui
  
  
  server <- function(input, output) {
    observe({
      if (input$file1 %>% is.null()) {
        NULL
      }
      else{
        inFile <- input$file1
        filename <<- sub('\\.tgz$', '', inFile$name)
        cat("filename:",
            inFile$name,
            "\n datapath: ",
            inFile$datapath,
            "\n")
        # keep only the directories in path
        myPath <<- gsub("(.*)/.*", "\\1", inFile$datapath)
        untar(inFile$datapath, exdir = myPath)
        
        cat(myPath, "contains untar of results archive  #####\n")
        metadataPath <- list.files(myPath, pattern = "\\.omr$", recursive = T)
        cat("****** omr path=>", metadataPath, "\n")
        dataPath <-  dir(path = myPath, pattern = "data", )
        cat("****** data path=>", dataPath, "\n")
        
        file.l <- list.files(dataPath)
       
        if (input$sizeLimiting) {
          fulldata <<- readOMResult(dataPath, sampleSize)
        } else{
          fulldata <<- readOMResult(dataPath, NULL)
        }
        cat(length(file.l), "results files extracted \n")
        meta_data <<- read_json(paste0(metadataPath, "/method.omr"))
        cat("omr description  file loaded \n")
        
        
      }#else file is null 
    
   
    
    output$metadataDisplay <- renderUI({
      if(! is.null(meta_data)){
        
        switch (meta_data$implementation,
          "StochasticPSE" = PSE_metadataDisplayer(meta_data),
          "StochasticNSGA2" = Evolution_metadataDisplayer(meta_data),
        )
      }
      else{
        h4("no meta data")
      }
      }) #renderUI
    
    })#observe
    
    output$fulldataDisplay <-  DT::renderDataTable({
        fulldata
    })
    
    
    output$inputSpace <- renderPlotly({
      SPLOMMaker(meta_data, fulldata, "input")
    })# renderPLot
    
    
    output$outputSpace <- renderPlotly({
      SPLOMMaker(meta_data, fulldata, "output")
    })#renderplot
    
    output$pingpong <- renderPlotly({
      pingpongPlot(meta_data, fulldata)
    })#renderplot
    
    
    output$objective_over_gen <- renderPlot({
      calibration_dynamic_plot_maker(meta_data, fulldata, input$make_obj_gen_Boxplot)
    })
    
  
    
  }#serverfunction
  
  
  # Run the application
  myPath <-  NULL
  keys <- NULL
  meta_data <-  NULL
  fulldata <- NULL
  filename <-  NULL
  shinyApp(ui = ui, server = server)
  
  
  
  
  #######################################################
  # Not Run Below this line 
  #########################################""
  
  
  # keep only the directories in path
  myPath <- "/tmp/"
  untar("~/dev/RopenMOLE/pse.tgz", exdir = myPath)
  
  cat(myPath, "contains untar of results archive  #####\n")
  metadataPath <- list.files(myPath, pattern = "\\.omr$", recursive = T, full.names = T)
  cat("****** omr path=>", metadataPath, "\n")
  dataPath <-  dir(path = myPath, recursive = T, pattern = "*ata*")
  cat("****** data path=>", dataPath, "\n")
  
  file.l <- list.files(dataPath)
  
  if (input$sizeLimiting) {
    fulldata <<- readOMResult(dataPath, sampleSize)
  } else{
    fulldata <<- readOMResult(dataPath, NULL)
  }
  cat(length(file.l), "results files extracted \n")
  meta_data <<- read_json(paste0(metadataPath, "/method.omr"))
  cat("omr description  file loaded \n")
  
  
    #export
  # library(htmlwidgets)
  # saveWidget(finalplot, "~/tmp/pingpong.html")
  
  
  
  
  #SPLOM by hand function helper
  
  #give  alls pairs of varibales given in list, no repeat, no symmetric
  #returns a list of couples
  # pair_combine  <- function(var_names, couples = list()) {
  #   if (length(var_names) == 2) {
  #     cat(var_names[1], "et", var_names[2], "\n")
  #     couples[[length(couples) + 1]] <-
  #       list(var_names[[1]], var_names[[2]])
  #     return(couples)
  #   }
  #   for (i in 2:length(var_names)) {
  #     cat(first(var_names), "et", var_names[i], "\n")
  #     couples[[length(couples) + 1]] <-
  #       list(var_names[[1]], var_names[[i]])
  #   }
  #   var_names <- var_names[-1]
  #   return(pair_combine(var_names, couples))
  # }
  # 
  # ins <-  pair_combine(genomeNames)
  # outs <-  pair_combine(objectivesNames)
  # 
  # #
  # 
  # 
  # trace_adding <- function(xformula, yformula , base_plotly_obj) {
  #   p1 <-  add_trace(
  #     base_plotly_obj,
  #     x = xformula,
  #     y = yformula,
  #     type = "scatter",
  #     mode = "markers",
  #     marker = list(size = 3, color =inputColor)
  #   ) %>% layout(dragmode = "lasso")
  #   return(p1)
  # }
  # 
  # pltly_list_maker <- function(pair, baseplt_ly, fulldata) {
  #   XX <-  pair[1] %>% as.character() %>% formulaMaker(fulldata)
  #   YY <-  pair[2] %>% as.character() %>% formulaMaker(fulldata)
  #   return(trace_adding(XX, YY, baseplt_ly))
  # }
  # lili <-  lapply(ins, pltly_list_maker, baseplt_ly, fulldata)
  
  
  
  
  
  
  
  
  