```{r}
# this function takes a 1024-row time series data set (of any number of columns) and finds their population standard deviations of each n-point mean
# in other words, it calculates the m-dispersion for each participant's time series data

all.mean.sds <- function(df){
# Z-score pop standard deviations:
sds0 <- apply(df, 2, sd.p)

# 2-point means:
means2pt <- as.data.frame(do.call(rbind,
        lapply(seq(1, nrow(df), 2), function(i){
          x <- df[ i:(i + 1), , drop = FALSE]
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

return(pop_sds)
}
