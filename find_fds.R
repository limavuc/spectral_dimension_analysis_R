```{r}
find_fds <- function(df, max_bin = 7, export = FALSE, filetype = ".txt"){
# function that turns raw time series data into the fractal dimensions of each series
# requires df, which is just a data frame containing the time series (singular or plural)
# ensure the data is in long format, such that each column is one participant and the rows consist of each time (in ms)
# automatically makes values >= abs(1000) NAs, pushes them to the end of the column, then deletes the NAs and truncates from the end to 1024 time data points
# NOTE: The cleaning function takes .txt files, but you can edit this to any type of file you have, so long as they are properly called
  
#input_data <- read.delim("*.txt")
  input_data <- abs(df)
  
# Replace values greater than or equal 1000 by default
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
# return(z_scores)
  
# Finding population standard deviations of z-scores:
  # Z-score pop standard deviations:
sds0 <- apply(z_scores, 2, sd.p)

# 2-point means:
means2pt <- as.data.frame(do.call(rbind,
        lapply(seq(1, nrow(z_scores), 2), function(i){
          x <- z_scores[ i:(i + 1), , drop = FALSE]
          res <- rbind(#x, 
                       colSums(x)/2)
          res
        })))
sds2 <- apply(means2pt, 2, sd.p)

# 4-point means:
means4pt <- as.data.frame(do.call(rbind,
        lapply(seq(1, nrow(means2pt), 2), function(i){
          x <- means2pt[ i:(i + 1), , drop = FALSE]
          res <- rbind(#x, 
                       colSums(x)/2)
         # rownames(res)[ nrow(res) ] <- paste(rownames(x), collapse = "_")
          res
        })))
rm(means2pt)
sds4 <- apply(means4pt, 2, sd.p)

# 8-point means:
means8pt <- as.data.frame(do.call(rbind,
        lapply(seq(1, nrow(means4pt), 2), function(i){
          x <- means4pt[ i:(i + 1), , drop = FALSE]
          res <- rbind(#x, 
                       colSums(x)/2)
         # rownames(res)[ nrow(res) ] <- paste(rownames(x), collapse = "_")
          res
        })))
rm(means4pt)
sds8 <- apply(means8pt, 2, sd.p)

# 16-point means:
means16pt <- as.data.frame(do.call(rbind,
        lapply(seq(1, nrow(means8pt), 2), function(i){
          x <- means8pt[ i:(i + 1), , drop = FALSE]
          res <- rbind(#x, 
                       colSums(x)/2)
         # rownames(res)[ nrow(res) ] <- paste(rownames(x), collapse = "_")
          res
        })))
rm(means8pt)
sds16 <- apply(means16pt, 2, sd.p)

# 32-point means:
means32pt <- as.data.frame(do.call(rbind,
        lapply(seq(1, nrow(means16pt), 2), function(i){
          x <- means16pt[ i:(i + 1), , drop = FALSE]
          res <- rbind(#x, 
                       colSums(x)/2)
         # rownames(res)[ nrow(res) ] <- paste(rownames(x), collapse = "_")
          res
        })))
rm(means16pt)
sds32 <- apply(means32pt, 2, sd.p)

# 64-point means:
means64pt <- as.data.frame(do.call(rbind,
        lapply(seq(1, nrow(means32pt), 2), function(i){
          x <- means32pt[ i:(i + 1), , drop = FALSE]
          res <- rbind(#x, 
                       colSums(x)/2)
         # rownames(res)[ nrow(res) ] <- paste(rownames(x), collapse = "_")
          res
        })))
rm(means32pt)
sds64 <- apply(means64pt, 2, sd.p)

# 128-point means:
means128pt <- as.data.frame(do.call(rbind,
        lapply(seq(1, nrow(means64pt), 2), function(i){
          x <- means64pt[ i:(i + 1), , drop = FALSE]
          res <- rbind(#x, 
                       colSums(x)/2)
         # rownames(res)[ nrow(res) ] <- paste(rownames(x), collapse = "_")
          res
        })))
rm(means64pt)
sds128 <- apply(means128pt, 2, sd.p)

# 256-point means:
means256pt <- as.data.frame(do.call(rbind,
        lapply(seq(1, nrow(means128pt), 2), function(i){
          x <- means128pt[ i:(i + 1), , drop = FALSE]
          res <- rbind(#x, 
                       colSums(x)/2)
         # rownames(res)[ nrow(res) ] <- paste(rownames(x), collapse = "_")
          res
        })))
rm(means128pt)
sds256 <- apply(means256pt, 2, sd.p)

# 512-point means:
means512pt <- as.data.frame(do.call(rbind,
        lapply(seq(1, nrow(means256pt), 2), function(i){
          x <- means256pt[ i:(i + 1), , drop = FALSE]
          res <- rbind(#x, 
                       colSums(x)/2)
         # rownames(res)[ nrow(res) ] <- paste(rownames(x), collapse = "_")
          res
        })))
rm(means256pt)
sds512 <- apply(means512pt, 2, sd.p)
rm(means512pt)

# deleting each means data frame to make things cleaner
#rm(means2pt, means4pt, means8pt, means16pt, means32pt, means64pt, means128pt, means256pt, means512pt)

# getting population standard deviations of each n^2-point mean in a data frame:
pop_sds <- data.frame(sds0, sds2, sds4, sds8, sds16, sds32, sds64, sds128, sds256, sds512)

# deleting each individual population standard deviation list
rm(sds0, sds2, sds4, sds8, sds16, sds32, sds64, sds128, sds256, sds512)

# fixing column names
names(pop_sds) <- c("Z-Score_SDs", "2-Point_Mean_SDs", "4-Point_Mean_SDs","8-Point_Mean_SDs","16-Point_Mean_SDs","32-Point_Mean_SDs","64-Point_Mean_SDs","128-Point_Mean_SDs","256-Point_Mean_SDs","512-Point_Mean_SDs")

# return(pop_sds)

# initializing an empty vector to store FDs
FD_values <- numeric(nrow(pop_sds))

# a for loop for calculating each FD by participant time series
  for(i in 1:nrow(pop_sds)){
    
# log_2 of each population sd by participant
log2s <- log2(pop_sds[i,1:max_bin])

# log_2 of each bin size (just a vector with length of max bins)
bin_sz_log2 <- c(1:max_bin) 

# combines the two vectors into one matrix
df_logs <- rbind(log2s, bin_sz_log2)

# takes the transpose of the above and turns it into a df for modeling
df_logs <- as.data.frame(t(df_logs))

# changes the variable names to something constant 
names(df_logs) <- c("Log_2_SDs", "Log_2_Bin_Sizes")

# un-quote this if you want to inspect the resulting data frames
# print(df_logs)

# fitting the model linearly
fit_mdl <- lm(Log_2_SDs ~ Log_2_Bin_Sizes, data = df_logs)

# extracting the slope
slope <- coef(fit_mdl)[2]

# fractal dimension
FD <- 1 - slope

# outputs each calculated FD into the initialized empty vector
FD_values[i] <- FD
  }

# binds all FDs into one data frame and returns the result
result <- data.frame(FD = FD_values)

if(export){
  if(file_type == "txt"){
    write.table(result, "FD_results.txt")
  } else if(file_type == "csv"){
    write.csv(result, "FD_results.csv")
  } else if(file_type == "xlsx"){
    write.xlsx(result, "FD_results.xlsx")
  } else{
    stop("Invalid file type specified! Use .txt, .csv, or .xlsx.")
  }
}

return(result)
}
```
