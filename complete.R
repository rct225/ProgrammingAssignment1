complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  file_id <- vector(length = length(id))
  nobs <- vector(length = length(id))
  
  file_index <- 1
  ## loop through the ids to access each file
  for(index in id) {
    ## pad the id with leading zeros to get the correct filename
    padded <- sprintf("%03d", index)
    
    ## create the filename by pasting together the directory and the padded id
    filename <- paste(directory, "/", padded, ".csv", sep="")
    
    ## open the current file
    current_data <- read.csv(file = filename, head=TRUE,sep=",")
    file_id[file_index] <- index
    nobs[file_index] <- sum(complete.cases(current_data))
    file_index <- file_index + 1
  }
  output_frame <- data.frame(file_id, nobs)
  colnames(output_frame) = c("id", "nobs")
  output_frame
}