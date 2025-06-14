#load data 
setwd("C:/Users/casey/OneDrive/Desktop/Data Analysis Learning")
originalData <- read.csv("Original_Conditions_Only.csv", header = TRUE)

#REMEMBER! PROPORTION CORRECT FOLLOWS A BINOMIAL DISTRIBUTION

#Run Binomial Test on each participant
for (i in 1:nrow(originalData)) {
  originalData$Score_p_value[i] <- binom.test(originalData$Score[i], originalData$LoopNumber[i], 0.5, alternative = "greater")$p.value
}

#Run aggregate binomial test across participants
binom.test(sum(originalData$Score), sum(originalData$LoopNumber), 0.5)$p.value

#Run Binomial test for each video
