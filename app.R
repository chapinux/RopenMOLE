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

SPLOMMaker <- function(meta_data, fulldata, type, baseplotly=NULL){
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
  fig <- NULL
  if(!is.null(baseplotly)){
    fig <- baseplotly
  }else{
  fig <-  plot_ly(data = fulldata)
  }
  fig <-  fig %>%  add_trace(
    type = 'splom',
    dimensions = splomDims,
    marker = list(size = 3,
                  color = if(type=="input"){"#29AF7F"}else{"#482677"})
  )
  fig <- fig %>% style(showupperhalf = F, diagonal=list(visible=FALSE))
  fig <-  fig %>%  layout(dragmode="lasso")
  return(fig)
}

calibration_dynamic_plot_maker <- function(meta_data, fulldata, makeBoxPlot){
  #last quarter objectives by generation time series plot
  
  gens <-  fulldata$evolution.generation %>% unique()
  lastquarter_gen <- quantile(gens)[4] 
  first_gen_last_quarter <- gens[which.min(abs(gens-lastquarter_gen ))]
  last_quarter_data <-  fulldata %>% filter(evolution.generation >= first_gen_last_quarter) %>% dplyr::arrange(evolution.generation)
  cat("lastquarter is ", nrow(last_quarter_data),"\n")
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

formulaMaker <-  function(x,fulldata){return(formula(fulldata %>% select(all_of(x))))}
# get formulas 

# prepare items of the dimensions attributs of plotly objectds
itemMaker <-  function(name, valueformula, fulldata){
  return(list(label=name, values=formulaMaker(name)))
}






pingpongPlot <- function(meta_data, fulldata){
  #check length of data , more than 6M is too much 
  if(fulldata %>% nrow() >= 6001 ){warning("too much data to handle ! Data reduced to 6000 lines\n")
    fulldata <- sample_n(fulldata, 6000)
  }
  
  
  genome <- unlist(meta_data$genome)
  genomeNames <- genome[names(genome)=="name"]
  
  objectivesNames <- meta_data$objective %>% unlist()  
  objectivesNames <- objectivesNames[names(objectivesNames) =="name"] %>% as.character()
  
  
  
  
  
  #plot preparation
  pp <-  highlight_key(fulldata)
  base <- plot_ly(pp, color = I("black"), showlegend = FALSE)
  
  #input subplot generation
  inputFormulas <-lapply(genomeNames, formulaMaker, fulldata)
  inputplt <-  list()
  while(length(inputFormulas)>0){
    if(length(inputFormulas)>1){
      formula1 <- inputFormulas[[1]]
      formula2 <- inputFormulas[[2]]
      # add scatterplot to list
      inputplt <-c(inputplt, add_markers(base, x=formula1, y=formula2,marker=list(color="#29AF7F")) %>% toWebGL() %>% list()) 
      inputFormulas <- inputFormulas[-c(1,2)]
    }
    if(length(inputFormulas)==1){
      #append histogramm to list
      formula1 <- inputFormulas[[1]]
      inputplt  <- c(inputplt,add_trace(base, x=formula1, type="histogram", marker=list(color="#29AF7F")) %>% toWebGL() %>% list()  )
    }
  }
  
  
  
  # p2 <-  add_markers(base, x=~i3, y=~i4) %>% toWebGL()
  # inputplt <-  list(p1,p2)
  
  #output subplot generation
  outputFormulas <-lapply(objectivesNames, formulaMaker, fulldata)
  outputplt <- list()
  while(length(outputFormulas)>0){
    if(length(outputFormulas)>1){
      formula1 <- outputFormulas[[1]]
      formula2 <- outputFormulas[[2]]
      # append scatterplot to list
      outputplt <-c(outputplt, add_markers(base, x=formula1, y=formula2, marker=list(color="#482677")) %>% toWebGL() %>% list()) 
      outputFormulas <- outputFormulas[-c(1,2)]
    }
    if(length(outputFormulas)==1){
      #add histogramm to list
      formula1 <- outputFormulas[[1]]
      outputplt  <- c(outputplt,add_trace(base, x=formula1, type="histogram", marker=list(color="#482677")) %>% toWebGL() %>% list() )
      outputFormulas <- outputFormulas[-1]
    }
  }
  
  
  #assemble like avengers
  finalplot <- subplot(subplot(inputplt,nrows = length(inputplt)),subplot(outputplt,nrows=length(outputplt)))
  finalplot <- layout(finalplot, dragmode="lasso")
  finalplot <- highlight(finalplot, on="plotly_selected", off="plotly_doubleclick")
  return(finalplot)
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
        tabPanel("Back and Forth",
                  plotlyOutput("pingpong") %>% withSpinner()),
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
  
  output$pingpong <- renderPlotly({
    pingpongPlot(meta_data,fulldata) 
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





fulldata <- sample_n(fulldata, 6000)

genome <- unlist(meta_data$genome)
genomeNames <- genome[names(genome)=="name"]
genomeLows  <- genome[names(genome)=="low"] %>% as.numeric()
genomehighs  <- genome[names(genome)=="high"] %>% as.numeric()
genomeimplem  <- genome[names(genome)=="implementation"]
genome <- data.frame(name=genomeNames,low=genomeLows,high=genomehighs,implementation=genomeimplem)


objectivesNames <- meta_data$objective %>% unlist()  
objectivesNames <- objectivesNames[names(objectivesNames) =="name"] %>% as.character()


 




#export
# library(htmlwidgets)
# saveWidget(finalplot, "~/tmp/pingpong.html")




#SPLOM by hand function helper 

#give  alls pairs of varibales given in list, no repeat, no symmetric 
#returns a list of couples
pair_combine  <- function(var_names, couples=list()){
  if (length(var_names) == 2) {
    cat(var_names[1], "et", var_names[2], "\n")
    couples[[length(couples) + 1]] <- list(var_names[[1]], var_names[[2]])
    return(couples)
  }
  for (i in 2:length(var_names)) {
    cat(first(var_names), "et", var_names[i], "\n")
    couples[[length(couples) + 1]] <- list(var_names[[1]], var_names[[i]])
  }
  var_names <- var_names[-1]
  return(pair_combine(var_names, couples))
}

ins <-  pair_combine(genomeNames)
outs <-  pair_combine(objectivesNames)

#


trace_adding <- function(xformula, yformula ,base_plotly_obj){
  p1 <-  add_trace(base_plotly_obj,
                   x=xformula,
                   y=yformula,
                   type="scatter",
                   mode="markers",
                   marker = list(size = 3, color = "#29AF7F")) %>% layout(dragmode="lasso")
  return(p1)
}

pltly_list_maker <- function(pair, baseplt_ly, fulldata){
  XX <-  pair[1] %>% as.character() %>% formulaMaker(fulldata)
  YY <-  pair[2] %>% as.character() %>% formulaMaker(fulldata)
  return(trace_adding(XX,YY,baseplt_ly))
}
lili <-  lapply(ins, pltly_list_maker, baseplt_ly,fulldata)
