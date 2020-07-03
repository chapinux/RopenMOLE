#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
# Load a json.gz file and oganize
library(jsonlite)
library(dplyr)
library(plotly)

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

readOMResult <- function(path){
    
  file.l <- list.files(path)
  result_data <- data.frame()
  for(i in 1:length(file.l)){
    
    tmp <-fromJSON(paste0(path,file.l[i])) %>% as.data.frame
    tmp$filenames <- file.l[i]
    result_data <- rbind(result_data,tmp)
  }
  result_data$ID <-  seq.int(from = 1,to=nrow(result_data))
  return(result_data)
}

inputsSPLOMMaker <- function(meta_data, simuData) {
  rawInputs <-  meta_data$genome
  dfInputsDescription <- data.frame()
  for (r in rawInputs) {
    dfInputsDescription <-rbind (dfInputsDescription, r %>% as.data.frame())
  }
  
  inputsNames <-  dfInputsDescription$value %>% as.character()
  inputsValues <- simuData %>% select(inputsNames)
  
  pltly_splom_dimlist_maker <-  function(name, simuData) {
    #the values to be named
    vv <- simuData[, name]
    ## the list formatted to fit ploty splom dimensions argument
    return(list(label = name, values = vv))
  }
  splomDims <-lapply(inputsNames, pltly_splom_dimlist_maker, inputsValues)
  
  #plotly creation
  fig <-  plot_ly(data = simuData)
  fig <-  fig %>%  add_trace(
    type = 'splom',
    dimensions = splomDims,
    marker = list(size = 7,
                  color = "red")
  )
  fig <- fig %>% style(showupperhalf = F)
  return(fig)
}


myPath <-  NULL
keys <- NULL
cradoGlobalK <-  NULL
meta_data <-  NULL
filename <-  NULL


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("R OpenMOLE"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
          
         fileInput("file1", "Choose OpenMole result file",
                   accept = c(
                     "tar.gz",
                     "tgz")
         )
       
         ,width = 2),
      
      mainPanel(
        
        tabsetPanel(
          tabPanel("MetaData",  uiOutput("tables")),
          tabPanel("Input-OutPut Spaces", 
                   fluidRow(
                     column(6,plotlyOutput("inputSpace")),
                     column(6, plotOutput("outputSpace"))
                   )
                   
                   ),#second tabPanel
          tabPanel("Full OM Data", tableOutput("fulldata"))
        )
      )#mainPanel
   )#sidebarlayout
)#ui

# Define server logic required to draw a histogram
server <- function(input, output) {
   observe({
   

  if(input$file1 %>% is.null()){NULL}
    else{
      inFile <- input$file1
      filename <<- sub('\\.tgz$', '', inFile$name) 
      myPath <<- gsub("(.*)/.*","\\1", inFile$datapath)
      untar(inFile$datapath, exdir = myPath)
    meta_data <<-read_json(paste0(myPath,"/", filename, "/method.omr"))
      cat("omr file loaded \n")
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
  

  
  
  output$fulldata <-  renderTable({
    readOMResult(paste0(myPath,"/", filename,"/data/"))
    })
  
  
  output$inputSpace <- renderPlotly({
     simuData <- readOMResult(paste0(myPath,"/", filename,"/data/"))
   cat("simulation data loaded in renderplotly")
      inputsSPLOMMaker(meta_data,simuData ) 
  })# renderPLot
  
  
  output$outputSpace <- renderPlot({
    plot(iris)
                                    })#renderplot


}#serverfunction 


# Run the application 
shinyApp(ui = ui, server = server)

# 
# 
# setwd("/home/chap/dev/RopenMOLE")
#  myPath <- "/tmp/resultbig/data/"
# xx <- readOMResult(myPath)
#  
# head(xx)
# inFile <- "result.tgz"
# untar(inFile, exdir = myPath)
# meta_data <<-read_json(paste0(myPath,"/result/method.omr"))
# keys <<-  names(meta_data)
# simuData <-  readOMResult(paste0(myPath,"/result/data/"))
# 





