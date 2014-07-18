rankhospital <- function(state, outcome, num = "best") {
  ##Load data and initial variables
  data <- read.csv("outcome-of-care-measures.csv", na.string="Not Available", stringsAsFactors=FALSE)
  allStates <- unique(data[,7])
  allOutcomes <- c("heart attack", "heart failure", "pneumonia")
  if(num == "best") {num=1}
  
  ##Validate arguments
  if(!is.element(state, allStates)) {
    stop("invalid state")
  } else {
    if(!is.element(outcome, allOutcomes)) {
      stop("invalid outcome")
    } else {
      
      ##Select ranked hospital
      stData <- data[data$State == state, ]
      if(outcome == "heart attack") {
        condition <- stData[order(stData[,11], stData[,2]),]
        condition2 <- na.omit(data.frame(condition[,c(2,11)]))        
        if(num=="worst") {num=nrow(condition2)}
        if(num <= nrow(condition2)) {
          print(condition2[num,1])
        } else {
          print(NA)
        }
      }else if(outcome == "heart failure") {
        condition <- stData[order(stData[,17], stData[,2]),]
        condition2 <- na.omit(data.frame(condition[,c(2,17)]))
        if(num=="worst") {num=nrow(condition2)}
        if (num <= nrow(condition2)) {
          print(condition2[num,1])
        } else {
          print(NA)
        }
      }else if(outcome == "pneumonia") {
        condition <- stData[order(stData[,23], stData[,2]),]
        condition2 <- na.omit(data.frame(condition[,c(2,23)]))
        if(num=="worst") {num=nrow(condition2)}
        if (num <= nrow(condition2)) {
          print(condition2[num,1])
        } else {
          print(NA)
        }
      }
    }
  }
}
