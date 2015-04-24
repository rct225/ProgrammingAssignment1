pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  ## variables to collect the sum and rows from all files
  total_sum <- 0
  total_rows <- 0
  
  ## loop through the ids to access each file
  for(index in id) {
    ## pad the id with leading zeros to get the correct filename
    padded <- sprintf("%03d", index)
    
    ## create the filename by pasting together the directory and the padded id
    filename <- paste(directory, "/", padded, ".csv", sep="")
    
    ## open the current file
    current_data <- read.csv(file = filename, head=TRUE,sep=",")
    
    ## get the data for the specified pollutant 
    pollutant_data <- current_data[pollutant]
    
    ## remove the NA entries
    data <- na.omit(pollutant_data)
    
    ## add the sum of the data from this file to the overall sum
    
    if (nrow(data) > 0 ) {
      total_sum <- total_sum + sum(data)
    }
    
    ## add the number of rows of the data from this files to the overall row count 
    total_rows <- total_rows + nrow(data)
  }
  
  ## return the mean by dividing the total_sum by the total_rows
  
  total_sum / total_rows
}