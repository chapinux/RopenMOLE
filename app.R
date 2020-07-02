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
           return(genome)
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
  # path <- ("~/dev/openmoleR/result/data/")
    file.l <- list.files(path)
  result_data <- data.frame()
  for(i in 1:length(file.l)){
    
    tmp <-fromJSON(paste0(path,file.l[i])) %>% as.data.frame
    tmp$filenames <- file.l[i]
    result_data <- rbind(result_data,tmp)
  }
  result_data$ID <-  seq.int(from = 1,to=nrow(result_data))
  cat(str(result_data), "####")
  return(result_data)
}



myPath <-  NULL
keys <- NULL
cradoGlobalK <-  NULL
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
       
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        tabsetPanel(
          tabPanel("MetaData",  uiOutput("tables")),
          tabPanel("Input-OutPut Spaces", 
                   fluidRow(
                     column(6,plotOutput("inputSpace")),
                     column(6, plotOutput("outputSpace"))
                   )
                   
                   ),#second tabPanel
          tabPanel("Full OM Data", tableOutput("fulldata"))
        )
         
        
      )#mainPanel
   )#sidebarlayout
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   observe({
   

  if(input$file1 %>% is.null()){NULL}
    else{
      inFile <- input$file1
      myPath <<- gsub("(.*)/.*","\\1", inFile$datapath)
      untar(inFile$datapath, exdir = myPath)

      meta_data <-read_json(paste0(myPath,"/result/method.omr"))
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
    readOMResult(paste0(myPath,"/result/data/"))
    })
  
  
  output$inputSpace <- renderPlot(
    plot(iris)
        # fig <-  plot_ly(data = meta_data)
    # fig <-  fig %>%  add_trace(
    #   
    #   type = 'splom',
    #   dimensions = list(
    #     list(label='sepal length', values=~sepal.length),
    #     list(label='sepal width', values=~sepal.width),
    #     list(label='petal length', values=~petal.length),
    #     list(label='petal width', values=~petal.width)
    #   ),
    #   text=~class,
    #   marker = list(
    #     color = as.integer(df$class),
    #     colorscale = pl_colorscale,
    #     size = 7,
    #     line = list(
    #       width = 1,
    #       color = 'rgb(230,230,230)'
    #     )
    #   )
    # )
    # 
    # fig <- fig %>%  style(diagonal = list(visible = F), showupperhalf = F)
)# renderPLot
  
  
  output$outputSpace <- renderPlot(plot(iris))


  
  extractinput <-  function() {} 
  
}#serverfunction 


# Run the application 
shinyApp(ui = ui, server = server)




