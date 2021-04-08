library(shiny)
# Load a json.gz file and oganize
library(jsonlite)
library(dplyr)
library(plotly)
library(data.table)
library(DT)
library(ggforce)
library(shinycssloaders)
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

calibration_dynamic_plot_maker <- function(meta_data, fulldata, makeBoxPlot){
  #last quarter objectives by generation time series plot
  
  gens <-  fulldata$evolution.generation %>% unique()
  lastquarter_gen <- quantile(gens)[4] 
  first_gen_last_quarter <- gens[which.min(abs(gens-lastquarter_gen ))]
  last_quarter_data <-  fulldata %>% filter(evolution.generation >= first_gen_last_quarter) %>% dplyr::arrange(evolution.generation)
  cat("lastquarte is ", nrow(last_quarter_data),"\n")
  # get objectives names
  # CAUTION dirty trick to extract duplicate names in named array 
  objectivesNames <- meta_data$objective %>% unlist()  
  objectivesNames <- objectivesNames[names(objectivesNames) =="name"] %>% as.character()
  
  #using ggforce facet matrix to obtain every objective versus genereation( which is ~ time of the exploration)
  obj_over_gen_plot <- ggplot(last_quarter_data)+
    geom_point(aes(x = .panel_x, y = .panel_y))+
    facet_matrix(rows=vars(all_of(objectivesNames)), cols = vars(evolution.generation))
  
  boxplot_by_gen <- ggplot(last_quarter_data)+
    geom_boxplot(aes(x = .panel_x, y = .panel_y, group=evolution.generation))+
    facet_matrix(rows=vars(all_of(objectivesNames)), cols = vars(evolution.generation))
  if(makeBoxPlot){
    print(boxplot_by_gen)
  }else{
  print(obj_over_gen_plot)
  }
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
        tabPanel("MetaData",  uiOutput("tables") %>% withSpinner()),
        tabPanel("Input-OutPut Spaces", 
                 fluidRow(
                   column(6,plotlyOutput("inputSpace") %>% withSpinner()),
                   column(6, plotlyOutput("outputSpace") %>% withSpinner())
                 )
                 
        ),#second tabPanel
        tabPanel("Objectives over time",
                 plotOutput("objective_over_gen", height= "250px") %>% withSpinner(),
                 checkboxInput("make_obj_gen_Boxplot", "draw boxplot by generation", value = FALSE)
                 ), #third tabpanel (objectives over time)
        tabPanel("Full result data",DT:: dataTableOutput("fulldata") %>% withSpinner())
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
      lapply(keys, tableOutput )
    }) #renderUI
  })#observe
  
  
  
  
  output$fulldata <-  DT::renderDataTable({
   fulldata
  })
  
  
  output$inputSpace <- renderPlotly({
    SPLOMMaker(meta_data,fulldata, "input" ) 
  })# renderPLot
  
  
  output$outputSpace <- renderPlotly({
    SPLOMMaker(meta_data,fulldata, "output" ) 
  })#renderplot
  
  
  output$objective_over_gen <- renderPlot({
    calibration_dynamic_plot_maker(meta_data, fulldata, input$make_obj_gen_Boxplot)
  })
  
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


genome <- unlist(meta_data$genome)
genomeNames <- genome[names(genome)=="name"]
genomeLows  <- genome[names(genome)=="low"] %>% as.numeric()
genomehighs  <- genome[names(genome)=="high"] %>% as.numeric()
genomeimplem  <- genome[names(genome)=="implementation"]
genome <- data.frame(name=genomeNames,low=genomeLows,high=genomehighs,implementation=genomeimplem)


objectivesNames <- meta_data$objective %>% unlist()  
objectivesNames <- objectivesNames[names(objectivesNames) =="name"] %>% as.character()

switch(key,
       "genome" = {
         genome <- as.data.frame()
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



#selection of inputs and outputs
# slct_O <-  c ("deltaFood1", "deltaFood2", "deltaFood3")
# slct_I <- c("i1", "i2", "i3", "i4")  
# 
# diagonalMatriceInput <- interaction(slct_I,slct_I, sep="vs.", lex.order = TRUE)
# combinaisons <- diagonalMatriceInput %>% levels() 
# combinaisons[-match(diagonalMatriceInput,combinaisons)]

