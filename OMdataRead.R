readOMResult <- function(path, sampleSize) {
  file.l <- list.files(path)
  result_data <- list()

  for (i in 1:length(file.l)) {
    tmp <- fromJSON(paste0(path, "/", file.l[i])) %>% as.data.frame
    tmp$filenames <- file.l[i]
    result_data[[i]] <- tmp
    if (i %% 100 == 0) {
      cat("file ", i, " / ", length(file.l), "\n")
    }
  }
  fulldata <- rbindlist(result_data)
  cat("concat ", length(file.l), "files in the archive\n")
  if (!is.null(sampleSize)) {
    fulldata <- sample_n(fulldata, sampleSize)
  }
  return(fulldata)
}
