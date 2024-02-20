```{r}
calculateFDs <- function(df, max_bin = 7, export=FALSE, file_type = "txt") {
# function that turns raw time series data into the fractal dimensions of each series
# requires df, which is just a data frame containing the time series (singular or plural)
# ensure the data is in long format, such that each column is one participant and the rows consist of each time (in ms)
# automatically makes values >= abs(1000) NAs, pushes them to the end of the column, then deletes the NAs and truncates from the end to 1024 time data points
# NOTE: The cleaning function takes .txt files, but you can edit this to any type of file you have, so long as they are properly called
  
########################################
# Cleaning steps
########################################
# Taking absolute value
df <- abs(df)

# Filtering any results above 1000
df[df >= 1000] <- NA

# shifting remaining cells upwards
df <- as.data.frame(lapply(df, function(x) c(x[!is.na(x)], x[is.na(x)])))

# truncating by selecting first 1024 time series data
df <- df[1:1024, ]
# renaming each column corresponding to the time series
names(df) <- paste0("Series", seq_len(ncol(df)))

########################################
# Finding population z-scores of each time series data
########################################
# only selecting the numeric columns, in case any non-numeric remain
numeric_columns <- sapply(df, is.numeric)

# finding population z-scores
z_scores <- as.data.frame(scale(df[, numeric_columns], scale = apply(df[, numeric_columns], 2, sd)))

# renaming z-scores columns
colnames(z_scores) <- paste0(colnames(df)[numeric_columns], "_Z")

# returning the z-scores

########################################
# Finding 2^n-point means
########################################
# ceiling just in case row count changes
powerOf2 <- ceiling(log(nrow(z_scores), 2))
Zmeans <-  lapply(
  seq_along(1:powerOf2),
    function(p2) {
      z_scores %>% 
        mutate(Group = rep(1:(nrow(.)/(2^p2)), each = 2^p2)) %>% 
        group_by(Group) %>% 
        # Adapt column selection according to your needs
        summarise(across(starts_with("Series"), \(.x) mean(.x, na.rm = TRUE))) %>% 
        add_column(PowerOf2 = p2)
    }
  ) %>% 
  bind_rows()

########################################
# Finding population standard deviations of 2^n-point means
########################################
# defining population standard deviation function
sd.p <- function(x){sd(x)*sqrt((length(x)-1)/length(x))}

# setting empty population standard deviation matrix
sdps <- matrix(NA, nrow = ncol(Zmeans), ncol = 9) 

# applying population standard deviations
for (i in 1:9) {
    # Calculate sds and assign them to the corresponding column of sdps
    sdps[, i] <- apply(Zmeans %>% filter(PowerOf2 == i), 2, sd.p)
}

# getting pop sds of the original z-scores
sdps0 <- apply(z_scores, 2, sd.p)
# binding the two pop sd matrices together
sdps <- cbind(sdps0, sdps)

# selecting out the grouping factors from the Zmeans output
sdps <- sdps[-c(1, nrow(sdps)), ]

# fixing row and column names
#label_col <- paste("Series", seq_len(nrow(sdps)), sep = "_")
#sdps <- as.data.frame(cbind(Time_Series = label_col, sdps))

sdps <- as.data.frame((sdps))
names(sdps) <- c("Z-Score_SDs", "2-Point_Mean_SDs", "4-Point_Mean_SDs","8-Point_Mean_SDs","16-Point_Mean_SDs","32-Point_Mean_SDs","64-Point_Mean_SDs","128-Point_Mean_SDs","256-Point_Mean_SDs","512-Point_Mean_SDs")


########################################
# Finding fractal dimensions
########################################

# initializing an empty vector to store FDs
FD_values <- numeric(nrow(sdps))

# a for loop for calculating each FD by participant time series
for(i in 1:nrow(sdps)){
    
# log_2 of each population sd by participant
log2s <- log2(sdps[i,1:7])

# log_2 of each bin size (just a vector with length of max bins)
bin_sz_log2 <- c(1:7) 

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
return(result)

########################################
# File output options
########################################

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

}
```
