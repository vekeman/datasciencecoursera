rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that the outcome is valid
  if(FALSE==outcome %in% c('heart attack','heart failure','pneumonia')){
    stop("invalid outcome")
  }
  ## Reduce to just theoutcome of interest
  if(outcome=='heart attack'){
    ##select only that outcome
    forStateComplete<-subset(outcomeData, select=c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
    ## apply generic names to columns
    names(forStateComplete) <- c("hospital", "state", "outcome_rate")
    ##convert to number
    forStateComplete[,3] <- suppressWarnings(as.numeric(forStateComplete[, 3]))
    ## drop NA
    forStateComplete<-forStateComplete[complete.cases(forStateComplete), ]
    ## sort will check for best or worst. if worst, will sort backward
    if(num=="worst"){
      forStateComplete<-forStateComplete[order(forStateComplete$state, -forStateComplete$outcome_rate, forStateComplete$hospital), ] 
    }else{
      forStateComplete<-forStateComplete[order(forStateComplete$state, forStateComplete$outcome_rate, forStateComplete$hospital), ] 
    }
    
  }
  if(outcome=='heart failure'){
    ##select only that outcome
    forStateComplete<-subset(outcomeData, select=c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))
    ## apply generic names to columns
    names(forStateComplete) <- c("hospital", "state", "outcome_rate")
    ##convert to number
    forStateComplete[,3] <- suppressWarnings(as.numeric(forStateComplete[, 3]))
    ## drop NA
    forStateComplete<-forStateComplete[complete.cases(forStateComplete), ]
    ## sort will check for best or worst. if worst, will sort backward
    if(num=="worst"){
      forStateComplete<-forStateComplete[order(forStateComplete$state, -forStateComplete$outcome_rate, forStateComplete$hospital), ] 
    }else{
      forStateComplete<-forStateComplete[order(forStateComplete$state, forStateComplete$outcome_rate, forStateComplete$hospital), ] 
    }
  }
  if(outcome=='pneumonia'){
    ##select only that outcome
    forStateComplete<-subset(outcomeData, select=c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
    ## apply generic names to columns
    names(forStateComplete) <- c("hospital", "state", "outcome_rate")
    ##convert to number
    forStateComplete[,3] <- suppressWarnings(as.numeric(forStateComplete[, 3]))
    ## drop NA
    forStateComplete<-forStateComplete[complete.cases(forStateComplete), ]
    ## sort will check for best or worst. if worst, will sort backward
    if(num=="worst"){
      forStateComplete<-forStateComplete[order(forStateComplete$state, -forStateComplete$outcome_rate, forStateComplete$hospital), ] 
    }else{
      forStateComplete<-forStateComplete[order(forStateComplete$state, forStateComplete$outcome_rate, forStateComplete$hospital), ] 
    }
  }
  
  
  ## For each state, find the hospital of the given rank
  
  ## add a column to rank order on
  x <- data.frame(rank_order = 1:nrow(forStateComplete))
  ##cbind it to the original data
  forStateComplete<-cbind(forStateComplete, x)
  ##set up two control variables and initialize them
  currentState<-"XX"
  currentRank<-1
  for(i in 1:nrow(forStateComplete)) {
    
    ##check to see if new state, if it is set the counter to 1
    if(currentState!=forStateComplete[i,2]){
      currentRank<-1
    } else {
      ## if not, increment the counter
      currentRank<-currentRank+1
    }
    ## assign the rank value
    forStateComplete[i,4]<-currentRank
    ##write the state value into the control variable
    currentState<-forStateComplete[i,2] 
    
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  if(num=="best" | num=="worst"){
    forStateComplete<-subset(forStateComplete, select=c("hospital","state"), rank_order==1)
  } else {
    forStateComplete<-subset(forStateComplete, select=c("hospital","state"), rank_order==num)
  }
  
  forStateComplete
  
}

