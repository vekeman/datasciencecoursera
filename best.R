## function to find the best hospital in a given state
best <- function(state, outcome) {
  
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(FALSE==state %in% outcomeData[,7]){
    stop("invalid state")
  }
  
  if(FALSE==outcome %in% c('heart attack','heart failure','pneumonia')){
    stop("invalid outcome")
  }
  if(outcome=='heart attack'){
    ##reduce to only records for that state as well as only complete cases
    forStateComplete<-subset(outcomeData, select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","State"), State==state)
    ##convert to number
    forStateComplete[,2] <- suppressWarnings(as.numeric(forStateComplete[, 2]))
    ## drop NA
    forStateComplete<-forStateComplete[complete.cases(forStateComplete), ]
    ## sort
    forStateComplete<-forStateComplete[order(forStateComplete$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, forStateComplete$Hospital.Name), ] 
  }
  if(outcome=='heart failure'){
    ##reduce to only records for that state as well as only complete cases
    forStateComplete<-subset(outcomeData, select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","State"), State==state)
    ##convert to number
    forStateComplete[,2] <- suppressWarnings(as.numeric(forStateComplete[, 2]))
    ## drop NA
    forStateComplete<-forStateComplete[complete.cases(forStateComplete), ]
    ## sort
    forStateComplete<-forStateComplete[order(forStateComplete$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, forStateComplete$Hospital.Name), ] 
  }
  if(outcome=='pneumonia'){
    ##reduce to only records for that state as well as only complete cases
    forStateComplete<-subset(outcomeData, select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia","State"), State==state)
    ##convert to number
    forStateComplete[,2] <- suppressWarnings(as.numeric(forStateComplete[, 2]))
    ## drop NA
    forStateComplete<-forStateComplete[complete.cases(forStateComplete), ]
    ## sort
    forStateComplete<-forStateComplete[order(forStateComplete$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, forStateComplete$Hospital.Name), ] 
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  forStateComplete[1,1]
}
