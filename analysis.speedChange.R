#load data 
setwd("C:/Users/casey/OneDrive/Desktop/Data Analysis Learning")
speedChangeData <- read.csv("SpeedChange_Only.csv", header = TRUE)

confidenceData <- speedChangeData

#CONFIDENCE ANALYSIS 

#Replace 1 - 6 values with 1 - 3 confidence values
  # 1 = low confidence
  # 2 = medium confidence
  # 3 = high confidence
for (i in 1:100) {
  col <- confidenceData[[i]]
  
  confidenceValues <- sapply(1:length(col), function(j) {
    value <- col[j]
    if (value == 1 || value == 6) {
      3
    } else if (value == 2 || value == 5) {
      2
    } else if (value == 3 || value == 4) {
      1
    } else {
      NA 
    }
  })
  
  confidenceData[[i]] <- confidenceValues
}

#new row with average confidence value


total_cols <- ncol(confidenceData)

avg_first_100 <- colMeans(confidenceData[, 1:100], na.rm = TRUE)

avg_rest <- rep(NA, total_cols - 100)
avg_row <- c(avg_first_100, avg_rest)

confidenceData <- rbind(confidenceData, avg_row)

rownames(confidenceData)[nrow(confidenceData)] <- "Column_Averages"

#VOLUNTARY/DIRECTED ANALYSIS
#Replace 1 - 6 values with 1 or 2 
# 1 = Directed
# 0 = Voluntary

intentionData <- speedChangeData

for (i in 1:100) {
  col <- intentionData[[i]]
  
  intentionValues <- sapply(1:length(col), function(j) {
    value <- col[j]
    if (value == 1 || value == 2 || value == 3) {
      1
    } else if (value == 4 || value == 5 || value == 6) {
      0
    } else {
      NA 
    }
  })
  
  intentionData[[i]] <- intentionValues
}

#Produce vectors of slow,regular and fast column names
slow_cols <- grep("^SLOW_", names(intentionData), value = TRUE)
reg_cols  <- grep("^REG_", names(intentionData), value = TRUE)
fast_cols <- grep("^FAST_", names(intentionData), value = TRUE)

#calculate proportion of 
intentionData$slow_prop <- rowMeans(intentionData[slow_cols], na.rm = TRUE)
intentionData$reg_prop  <- rowMeans(intentionData[reg_cols], na.rm = TRUE)
intentionData$fast_prop <- rowMeans(intentionData[fast_cols], na.rm = TRUE)

#t-tests

t.test(intentionData$slow_prop, intentionData$reg_prop, paired = TRUE)  # Slow vs. Regular
t.test(intentionData$reg_prop, intentionData$fast_prop, paired = TRUE)  # Regular vs. Fast

