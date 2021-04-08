library(shiny)
# Load a json.gz file and oganize
library(jsonlite)
library(dplyr)
library(plotly)
library(data.table)
library(DT)

#set size limit for results files input 
maxMegaBytesSize <-  50 
options(shiny.maxRequestSize=maxMegaBytesSize*1024^2)


#set size of displayed sample 
sampleSize <-  10000


#utility function to generate dataframe from OM metadata archive to ease display in shiny  

dataframe_factory <-  function(key, meta_data){
  switch(key,
         "genome" = {
           genome <- as.data.frame(t(unlist(meta_data$genome)))
           genome <- (rbind(genome[,1:4], genome[,5:8]))
           return(genome %>% as.data.frame())
         },
         "objective" ={
           df <-  as.data.frame(t(unlist(meta_data[[key]])))
           return(df)
         },
         "saved" = {
           df <-  as.data.frame(t(unlist(meta_data[[key]])))
           return(df)
         },
         "mu"= {meta_data[[key]]},
         "sample"={meta_data[[key]]},
         "implementation" ={meta_data[[key]]},
         "method"={meta_data[[key]]}
  )#switch
}#dataframe factory

readOMResult <- function(path, sampleSize){
  file.l <- list.files(path)
  result_data <- list()
  for(i in 1:length(file.l)){
    tmp <-fromJSON(paste0(path,"/",file.l[i])) %>% as.data.frame
    tmp$filenames <- file.l[i]
    result_data[[i]] <-tmp
    if(i %% 100 ==0){cat("file ", i," / ", length(file.l), "\n")}
  }
  fulldata <- rbindlist(result_data)
  cat("concat ",length(file.l), "files in the archive\n")
  if(! is.null(sampleSize)){
  fulldata <- sample_n(fulldata, sampleSize)
  }
  return(fulldata)
}

SPLOMMaker <- function(meta_data, fulldata, type){
  #getting inputs name in case of NSGA2 calibration
  if(type =="input"){
  rawDimNames <-  meta_data$genome
  }
  if(type =="output"){
    rawDimNames <-  meta_data$objective
  }
  
  dfDimDescription <- data.frame()
  for (r in rawDimNames) {
    dfDimDescription <-rbind (dfDimDescription, r %>% as.data.frame())
  }
  DimLabels <-  dfDimDescription$name %>% as.character()
  
  # plotly needs formulas to feed the SPLOM graph
  # function to convert dataframe column to formula
  formulaMaker <-  function(x){return(formula(fulldata %>% select(all_of(x))))}
  # get formulas 
  splomDimsFormulas <-lapply(DimLabels, formulaMaker)
  
  # prepare items of the dimensions atytributs of plotly objectds
  itemMaker <-  function(name, valueformula){
    return(list(label=name, values=formulaMaker(name)))
  }
  # make a list of couples (name, formula)
  splomDims <- lapply(DimLabels,itemMaker, splomDimsFormulas )
  
  #plotly creation
  fig <-  plot_ly(data = fulldata)
  fig <-  fig %>%  add_trace(
    type = 'splom',
    dimensions = splomDims,
    marker = list(size = 3,
                  color = "red")
  )
  fig <- fig %>% style(showupperhalf = F, diagonal=list(visible=FALSE))
  return(fig)
}




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("R OpenMOLE"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      fileInput("file1", "Choose OpenMole result file",
                multiple=F,
                placeholder = "No more than 50 MB",
                accept = c(
                  "tar.gz",
                  "tgz")
      ),
      checkboxInput("sizeLimiting", "limit display to 10000 sampled points", value = FALSE, width = NULL)
      
      ,width = 2),
    
    mainPanel(
      
      tabsetPanel(
        tabPanel("MetaData",  uiOutput("tables")),
        tabPanel("Input-OutPut Spaces", 
                 fluidRow(
                   column(6,plotlyOutput("inputSpace")),
                   column(6, plotlyOutput("outputSpace"))
                 )
                 
        ),#second tabPanel
        tabPanel("Objectives over time"), #third tabpanel
        tabPanel("Full result data",DT:: dataTableOutput("fulldata"))
      )
    )#mainPanel
  )#sidebarlayout
)#ui


server <- function(input, output) {
  observe({
    if(input$file1 %>% is.null()){NULL}
    else{
      inFile <- input$file1
      filename <<- sub('\\.tgz$', '', inFile$name) 
      cat("filename:", inFile$name, "\n datapath", inFile$datapath, "\n" )
      # keep only the directories in path
      myPath <<- gsub("(.*)/.*","\\1", inFile$datapath)
      cat(myPath, "#####\n")
      untar(inFile$datapath, exdir = myPath)
      dataPath <-  paste0(myPath,"/result/data")
      metadataPath <-  paste0(myPath,"/result")
      file.l <- list.files(dataPath)
      cat(length(file.l), "results files extracted \n")
      if(input$sizeLimiting){
      fulldata <<- readOMResult( dataPath, sampleSize)
      }else{
      fulldata <<- readOMResult( dataPath, NULL)
      }
      meta_data <<-read_json(paste0(metadataPath,"/method.omr"))
      cat("omr description  file loaded \n")
      keys <<-  names(meta_data)
      
    }#else
    
    if(!is.null(keys)){
      lapply(keys, function(k) {
        tatable <-  dataframe_factory(k, meta_data)
        output[[k]] <-  renderTable({tatable})
      }) #lapply
    }#if not null
    
    output$tables <- renderUI({
      lapply(keys, tableOutput)
    }) #renderUI
  })#observe
  
  
  
  
  output$fulldata <-  DT::renderDataTable({
   fulldata,
    server=TRUE
  })
  
  
  output$inputSpace <- renderPlotly({
    SPLOMMaker(meta_data,fulldata, "input" ) 
  })# renderPLot
  
  
  output$outputSpace <- renderPlotly({
    SPLOMMaker(meta_data,fulldata, "output" ) 
  })#renderplot
  
  
}#serverfunction 


# Run the application 
myPath <-  NULL
keys <- NULL
cradoGlobalK <-  NULL
meta_data <-  NULL
fulldata <- NULL
filename <-  NULL
shinyApp(ui = ui, server = server)

# 
# 
setwd("/home/paulchapron/dev/RopenMOLE")
myPath <- "./result/data/"
file.l <- list.files(myPath)
cat(length(file.l), "files in the archive ")
result_data <- list()
for(i in 1:length(file.l)){
  tmp <-fromJSON(paste0(myPath,file.l[i])) %>% as.data.frame
  tmp$filenames <- file.l[i]
  result_data[[i]] <-tmp
  if(i %% 100 ==0){cat("file ", i," / ", length(file.l), "\n")}
}


fulldata <- rbindlist(result_data)
# untar(inFile, exdir = myPath)

meta_data <<-read_json("./result/method.omr")
keys <<-  names(meta_data)

# 

# inputsSPLOMMaker <- function(meta_data, fulldata) {
#getting inputs name in case of NSGA2

rawInputs <-  meta_data$genome
rawOutputs <- meta_data$objective

dfInputsDescription <- data.frame()
for (r in rawInputs) {
  dfInputsDescription <-rbind (dfInputsDescription, r %>% as.data.frame())
}
inputsNames <-  dfInputsDescription$name %>% as.character()

# plotly needs formulas to feed the SPLOM graph
# function to convert dataframe column to formula
formulaMaker <-  function(x){return(formula(fulldata %>% select(all_of(x))))}
# get formulas 
splomDimsFormulas <-lapply(inputsNames, formulaMaker)

# prepare items of the dimensions atytributs of plotly objectds
itemMaker <-  function(name, valueformula){
  return(list(label=name, values=formulaMaker(name)))
}
# make a list of couples (name, formula)
splomDims <- lapply(inputsNames,itemMaker, splomDimsFormulas )

#plotly creation
fig <-  plot_ly(data = fulldata)
fig <-  fig %>%  add_trace(
  type = 'splom',
  dimensions = splomDims,
  marker = list(size = 3,
                color = "red")
)
fig <- fig %>% style(showupperhalf = F, diagonal=list(visible=FALSE))
return(fig)
# }





#selection of inputs and outputs
slct_O <-  c ("deltaFood1", "deltaFood2", "deltaFood3")
slct_I <- c("i1", "i2", "i3", "i4")  

diagonalMatriceInput <- interaction(slct_I,slct_I, sep="vs.", lex.order = TRUE)
combinaisons <- diagonalMatriceInput %>% levels() 
combinaisons[-match(diagonalMatriceInput,combinaisons)]

