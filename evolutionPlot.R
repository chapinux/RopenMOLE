
# get formulas from variable names  for pltly splom and traces 
formulaMaker <-   function(x, fulldata) {
  return(formula(fulldata %>% select(all_of(x))))
}


# prepare items of the dimensions attributs of plotly objectds
itemMaker <-  function(name, valueformula, fulldata) {
  return(list(label = name, values = formulaMaker(name)))
}

# plot matrix lowerhalf
SPLOMMaker <- function(meta_data, fulldata, type, baseplotly = NULL) {
  #getting inputs name in case of NSGA2 calibration
  if (type == "input") {
    rawDimNames <-  meta_data$genome
  }
  if (type == "output") {
    rawDimNames <-  meta_data$objective
  }
  
  dfDimDescription <- data.frame()
  for (r in rawDimNames) {
    dfDimDescription <- rbind (dfDimDescription, r %>% as.data.frame())
  }
  DimLabels <-  dfDimDescription$name %>% as.character()
  
  # plotly needs formulas to feed the SPLOM graph
  # function to convert dataframe column to formula
  formulaMaker <-
    function(x) {
      return(formula(fulldata %>% select(all_of(x))))
    }
  # get formulas
  splomDimsFormulas <- lapply(DimLabels, formulaMaker)
  
  # prepare items of the dimensions atytributs of plotly objectds
  itemMaker <-  function(name, valueformula) {
    return(list(label = name, values = formulaMaker(name)))
  }
  # make a list of couples (name, formula)
  splomDims <- lapply(DimLabels, itemMaker, splomDimsFormulas)
  
  #plotly creation
  fig <- NULL
  if (!is.null(baseplotly)) {
    fig <- baseplotly
  } else{
    fig <-  plot_ly(data = fulldata)
  }
  fig <-  fig %>%  add_trace(
    type = 'splom',
    dimensions = splomDims,
    marker = list(size = 3,
                  color = if (type == "input") {
                    inputColor
                  } else{
                    outputColor
                  })
  )#splomtrace
  fig <-  fig %>% style(showupperhalf = F,
                        diagonal = list(visible = FALSE))
  fig <-  fig %>%  layout(dragmode = "lasso")
  return(fig)
}


#last quarter objectives by generation time series plot
calibration_dynamic_plot_maker <-   function(meta_data, fulldata, makeBoxPlot) {
  gens <-  fulldata$evolution.generation %>% unique()
  lastquarter_gen <- quantile(gens)[4]
  first_gen_last_quarter <-
    gens[which.min(abs(gens - lastquarter_gen))]
  last_quarter_data <-
    fulldata %>% filter(evolution.generation >= first_gen_last_quarter) %>% dplyr::arrange(evolution.generation)
  cat("lastquarter is ", nrow(last_quarter_data), "\n")
  # get objectives names
  # CAUTION dirty trick to extract duplicate names in named array
  objectivesNames <- meta_data$objective %>% unlist()
  objectivesNames <-
    objectivesNames[names(objectivesNames) == "name"] %>% as.character()
  
  #using ggforce facet matrix to obtain every objective versus genereation( which is ~ time of the exploration)
  obj_over_gen_plot <- ggplot(last_quarter_data) +
    geom_point(aes(x = .panel_x, y = .panel_y)) +
    facet_matrix(rows = vars(all_of(objectivesNames)),
                 cols = vars(evolution.generation))
  
  boxplot_by_gen <- ggplot(last_quarter_data) +
    geom_boxplot(aes(x = .panel_x, y = .panel_y, group = evolution.generation)) +
    facet_matrix(rows = vars(all_of(objectivesNames)),
                 cols = vars(evolution.generation))
  if (makeBoxPlot) {
    print(boxplot_by_gen)
  } else{
    print(obj_over_gen_plot)
  }
}

pingpongPlot <- function(meta_data, fulldata) {
  #check length of data , more than 6M is too much
  if (fulldata %>% nrow() >= 6001) {
    warning("too much data to handle ! Data reduced to 6000 lines\n")
    fulldata <- sample_n(fulldata, 6000)
  }
  
  
  genome <- unlist(meta_data$genome)
  genomeNames <- genome[names(genome) == "name"]
  
  objectivesNames <- meta_data$objective %>% unlist()
  objectivesNames <-
    objectivesNames[names(objectivesNames) == "name"] %>% as.character()
  
  #plot preparation
  pp <-  highlight_key(fulldata)
  base <- plot_ly(pp, color = I("black"), showlegend = FALSE)
  
  #input subplot generation
  inputFormulas <- lapply(genomeNames, formulaMaker, fulldata)
  inputplt <-  list()
  while (length(inputFormulas) > 0) {
    if (length(inputFormulas) > 1) {
      formula1 <- inputFormulas[[1]]
      formula2 <- inputFormulas[[2]]
      # add scatterplot to list
      inputplt <-
        c(
          inputplt,
          add_markers(
            base,
            x = formula1,
            y = formula2,
            marker = list(color = inputColor)
          ) %>% toWebGL() %>% list()
        )
      inputFormulas <- inputFormulas[-c(1, 2)]
    }
    if (length(inputFormulas) == 1) {
      #append histogramm to list
      formula1 <- inputFormulas[[1]]
      inputplt  <-
        c(
          inputplt,
          add_trace(
            base,
            x = formula1,
            type = "histogram",
            marker = list(color = inputColor)
          ) %>% toWebGL() %>% list()
        )
    }
  }
  
  
  #output subplot generation
  outputFormulas <- lapply(objectivesNames, formulaMaker, fulldata)
  outputplt <- list()
  while (length(outputFormulas) > 0) {
    if (length(outputFormulas) > 1) {
      formula1 <- outputFormulas[[1]]
      formula2 <- outputFormulas[[2]]
      # append scatterplot to list
      outputplt <-
        c(
          outputplt,
          add_markers(
            base,
            x = formula1,
            y = formula2,
            marker = list(color = outputColor)
          ) %>% toWebGL() %>% list()
        )
      outputFormulas <- outputFormulas[-c(1, 2)]
    }
    if (length(outputFormulas) == 1) {
      #add histogramm to list
      formula1 <- outputFormulas[[1]]
      outputplt  <-
        c(
          outputplt,
          add_trace(
            base,
            x = formula1,
            type = "histogram",
            marker = list(color = outputColor)
          ) %>% toWebGL() %>% list()
        )
      outputFormulas <- outputFormulas[-1]
    }
  }
  
  
  #assemble like avengers
  finalplot <-
    subplot(subplot(inputplt, nrows = length(inputplt)),
            subplot(outputplt, nrows = length(outputplt)))
  finalplot <- layout(finalplot, dragmode = "lasso")
  finalplot <-
    highlight(finalplot, on = "plotly_selected", off = "plotly_doubleclick")
  return(finalplot)
}


