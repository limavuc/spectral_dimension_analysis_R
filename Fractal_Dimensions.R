```{r}
FD.calculate <- function(X, max_bin){

# This calculates the fractal dimensions of data already translated into Z-score population standard deviations of time series data n-point means. Please use another function I have created if this is not yet the case.
# X = the population standard deviation data frame, can be any number of rows.
# max_bin = the maximum bin number you want to take the linear model to. 7 is usually standard.

# initializing an empty vector to store FDs
FD_values <- numeric(nrow(X))

# a for loop for calculating each FD by participant time series
  for(i in 1:nrow(X)){
    
# log_2 of each population sd by participant
log2s <- log2(X[i,1:max_bin])

# log_2 of each bin size (just a vector with length of max bins)
bin_sz_log2 <- c(1:max_bin) 

# combines the two vectors into one matrix
df_logs <- rbind(log2s, bin_sz_log2)

# takes the transpose of the above and turns it into a df for modeling
df_logs <- as.data.frame(t(df_logs))

# changes the variable names to something constant 
names(df_logs) <- c("Log_2_SDs", "Log_2_Bin_Sizes")

# un-quote this if you want to inspect the resulting data frames
#print(df_logs)

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
}
```
