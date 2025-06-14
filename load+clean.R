#access data set and remove unused rows/columns
setwd("C:/Users/casey/OneDrive/Desktop/Data Analysis Learning/SlowMotionAnalysis")
data <- read.csv("Test.Analysis.csv", header = TRUE, skip = 0)
data <- data[-1, ]
data <- data[, -c(1:19)]

#filter into conditions
data_original <- subset(data, Condition == "Original")
data_speedchange <- subset(data, Condition == "SpeedChange")

#removed unused trials
data_original <- data_original[, -c(1:100)]
data_speedchange <- data_speedchange[, -c(101:200)]

#create vector of 100 names via sapply on a vector of 1-100
speed_names <- sapply(1:100, function(i) {
  if (i %% 3 == 1) {
    paste0("FAST_", i)
  } else if (i %% 3 == 2) {
    paste0("SLOW_", i)
  } else if (i %% 3 == 0) {
    paste0("REG_", i)
  }
})

# assigning speed names to 
colnames(data_speedchange)[1:100] <- speed_names

#write output files
write.csv(data_speedchange, "SpeedChange_Only.csv", row.names = FALSE)
write.csv(data_original, "Original_Conditions_Only.csv", row.names = FALSE)

