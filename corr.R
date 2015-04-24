corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  my_vector <- vector()
  for (id in list.files(directory)){
    ## create the filename by pasting together the directory and the padded id
    filename <- paste(directory, "/", id, sep="")
    
    ## open the current file
    current_data <- read.csv(file = filename, head=TRUE,sep=",")
    
    if (sum(complete.cases(current_data)) > threshold) {
      my_cor <- cor(current_data["nitrate"], current_data["sulfate"], use = "na.or.complete")
      my_vector <- append(my_vector, my_cor)
    }
  }
  my_vector
}