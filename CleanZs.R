pacman::p_load(tidyverse)

CleanZs <- function(input_data)	{
# function that cleans the time series data
# requires input_data, which is just a data frame containing the time series (singular or plural)
# ensure the data is in long format, such that each column is one participant and the rows consist of each time (in ms)
# automatically makes values >= abs(1000) NAs, pushes them to the end of the column, then deletes the NAs and truncates from the end to 1024 time data points
# NOTE: The cleaning function takes .txt files, but you can edit this to any type of file you have, so long as they are properly called
  
#input_data <- read.delim("*.txt")
  input_data <- abs(input_data)
  
# Replace values greater than or equal 1000
  input_data[input_data >= 1000] <- NA
  
# Shift remaining cells up in each column
  input_data <- as.data.frame(lapply(input_data, function(x) c(x[!is.na(x)], x[is.na(x)])))
  
# Select the first 'final_rows' rows
  input_data <- input_data[1:1024, ]
  
# rename each column with "Participant" and the number of the column
  names(input_data) <- paste0("Participant", seq_len(ncol(input_data)))

  

## Finding population z-scores:
  
  
# Get the numeric columns (excluding non-numeric columns like participant or trial)
  numeric_columns <- sapply(input_data, is.numeric)
  
# Apply z-score transformation with population standard deviations to numeric columns
  z_scores <- as.data.frame(scale(input_data[, numeric_columns], scale = apply(input_data[, numeric_columns], 2, sd)))
  
# Rename the z-score columns
  colnames(z_scores) <- paste0(colnames(input_data)[numeric_columns], "_Z")
  
# return the z-scores
return(z_scores)
}
