```{r}
plot_disper <- function(X, timeseriesnumber, max_bin){
  # X = the data frame containing your population SDS
  # time series = which time series you want to plot
  # max_bin = the max bin you want to plot to
X$name <- seq.int(nrow(X))
X <- subset(X, name == timeseriesnumber)
X <- X[1:max_bin]
X <- as.data.frame(t(X))
log2s <- log2(X)
print(log2s)

# log_2 of each bin size (just a vector with length of max bins)
bin_sz_log2 <- c(1:max_bin) 

# combines the two vectors into one matrix
df_logs <- cbind(log2s, bin_sz_log2)

# takes the transpose of the above and turns it into a df for modeling
df_logs <- as.data.frame(df_logs)

# changes the variable names to something constant 
names(df_logs) <- c("Log_2_SDs", "Log_2_Bin_Sizes")
print(df_logs)

lm_mdl <- lm(Log_2_SDs ~ Log_2_Bin_Sizes, data = df_logs)
slope <- coef(lm_mdl)[2]
DF <- 1 - slope
DF <- as.data.frame(DF)

ggplot(data = df_logs, aes(x = Log_2_Bin_Sizes, y = Log_2_SDs)) + 
  geom_point(color="gray", size = 0.5) + 
  geom_smooth(method = "lm", color = "black") +
  xlab("Log_2 of base-2 bin sizes") +
  ylab(expression("Log_2(SD)")) +
  title("Regression fit") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")))
}
```
