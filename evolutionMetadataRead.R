metadataDisplayer <- function(meta_data){
  genome <- unlist(meta_data$genome)
  genomeNames <- genome[names(genome) == "name"]
  genomeLows  <- genome[names(genome) == "low"] %>% as.numeric()
  genomehighs  <- genome[names(genome) == "high"] %>% as.numeric()
  genomeimplem  <- genome[names(genome) == "implementation"]
  genome <- data.frame(
    name = genomeNames,
    low = genomeLows,
    high = genomehighs,
    implementation = genomeimplem
  )
  
  
  objectives <- meta_data$objective %>% unlist()
  objectivesNames <- objectives[names(objectives) == "name"] %>% as.character()
  objectivesNegative <- objectives[names(objectives) == "negative"] %>% as.logical()
  objectives <-  data.frame(name=objectivesNames, negative=objectivesNegative)
  
  
  
  
  saveOptions <- meta_data$`save-option` %>% unlist()
  saveOptionsFreq <- saveOptions[names(saveOptions)=="frequency"] %>% as.numeric()
  saveOptionsLast <- saveOptions[names(saveOptions)=="last"] %>% as.logical()
  saveOptions <-  data.frame(frequency=saveOptionsFreq, last=saveOptionsLast)
  
  
  
  metadata_list_display <-  list(
    h3(paste0(meta_data$method," results (", meta_data$implementation, ")")),
    h4("genome"),
    renderTable(genome), 
    h4("objectives"),
    renderTable(objectives),
    hr(),
    h4("Details"),
    p("population size TODO  "),
    renderText(meta_data$`population-size`), 
    br(),
    p("percentage of solution re-evaluated  TODO" ),
    renderText(meta_data$sample),
    br(),
    p(paste0("generation reached TODO :", meta_data$generation)),
    br(),
    p("save options "),
    renderTable(saveOptions), 
    h4("method infos:"),
    p(paste0("implementation : ", meta_data$implementation)),
    br(),
    p(paste0("method :", meta_data$method)),
    br(),
    p("results location inside archive"), 
    renderText(meta_data$data),
    br(),
    p(paste0("version : ",meta_data$version))
  )#list
  
  return(metadata_list_display)
  
}
