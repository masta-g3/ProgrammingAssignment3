best <- function(state, outcome) {
  ##Load data and initial variables
  data <- read.csv("outcome-of-care-measures.csv", na.string="Not Available", stringsAsFactors=FALSE)
  allStates <- unique(data[,7])
  allOutcomes <- c("heart attack", "heart failure", "pneumonia")

  ##Validate arguments
  if(!is.element(state, allStates)) {
    stop("invalid state")
  } else {
    if(!is.element(outcome, allOutcomes)) {
      stop("invalid outcome")
    } else {

      ##Select best hospital
      stData <- data[data$State == state, ]
      if(outcome == "heart attack") {
        condition <- stData[order(stData[,11], stData[,7]),]
        condition2 <- data.frame(t(condition))
        paste(condition2[2,1])
      } else if(outcome == "heart failure") {
        condition <- stData[order(stData[,17], stData[,7]),]
        condition2 <- data.frame(t(condition))
        paste(condition2[2,1])
      } else if(outcome == "pneumonia") {
        condition <- stData[order(stData[,23], stData[,7]),]
        condition2 <- data.frame(t(condition))
        paste(condition2[2,1])       
      }      
    }
  }
}