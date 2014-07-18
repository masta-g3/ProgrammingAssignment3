rankall <- function(outcome, num = "best") {
  
  ##Load data and check variables
  data <- read.csv("outcome-of-care-measures.csv", na.string="Not Available", stringsAsFactors=FALSE)
  allStates <- unique(data[,7])
  allOutcomes <- c("heart attack", "heart failure", "pneumonia")
  
  #Interpet passed variables
  if(num == "best") {num=1}
  
  if(!is.element(outcome, allOutcomes)) {
    stop("invalid outcome")
  } else if(outcome=="heart attack") {
    n=11
  } else if(outcome=="heart failure"){
    n=17
  } else if(outcome=="pneumonia")    {
    n=23
  }
  
  ##Rank by outcome and name, then split by state
  rkData <- data[order(data[,n], data[,2]),]
  rkData2 <- na.omit(data.frame(rkData[,c(2,7,n)]))
  ##stData <- 
  by(rkData2, rkData$State, function(num) print(rkData2[num,2]))
  ##head(stData)
}
  