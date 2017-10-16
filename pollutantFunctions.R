##my file path is here 
## C:/Users/Redirection/vekemr2/Desktop/Non OM4/Data Science/r programing/week 2/specdata

pollutantmean <- function(directory, pollutant, id=1:332){
  
  for(i in id){  
    ## create file name with leading zeros
    if(i<10){
      idTxt <- paste(c('00', i), collapse = "")
    }
    else if(i<100){
      idTxt <- paste(c('0',i), collapse = "")
    } 
    else {idTxt <- i}
    
    ##print(paste(c(directory,'/', idTxt, '.csv'), collapse = ""))
    
    
    ## load current file
    currFile <-  read.csv(paste(c(directory,'/', idTxt, '.csv'), collapse = ""))
    ##print(currFile)
    
    #stacktomainfile - if data frame not yet created, create it
    if(exists("pollDataFrame")){
      pollDataFrame<-rbind(pollDataFrame, currFile)
    }
    else{
      pollDataFrame<-currFile
    }
  
  }
  
  ##perform mean
  if(pollutant=='sulfate'){
    myResult<-mean(pollDataFrame$sulfate, na.rm=TRUE)
  }
  else if(pollutant=='nitrate'){
    myResult<-mean(pollDataFrame$nitrate, na.rm=TRUE)
  } ##exit loop 
  myResult
}


complete <- function(directory, id=1:332){
  for(i in id){  
    ## create file name with leading zeros
    if(i<10){
      idTxt <- paste(c('00', i), collapse = "")
    }
    else if(i<100){
      idTxt <- paste(c('0',i), collapse = "")
    } 
    else {idTxt <- i}
    
    ##print(paste(c(directory,'/', idTxt, '.csv'), collapse = ""))
    
    
    ## load current file
    currFile <-  read.csv(paste(c(directory,'/', idTxt, '.csv'), collapse = "")) 
    
    
    #stacktomainfile - if data frame not yet created, create it
    if(exists("pollDataFrame")){
      pollDataFrame<-rbind(pollDataFrame, data.frame(id = i, nobs=sum(complete.cases(currFile))))
      ##print("exist pass")
    }
    else{
      pollDataFrame<-data.frame(id = i, nobs=sum(complete.cases(currFile)))
    }
    
  } ##exit loop 
  pollDataFrame
}

corr <-function(directory, threshold=0){
  for(i in 1:332){  
    ## create file name with leading zeros
    if(i<10){
      idTxt <- paste(c('00', i), collapse = "")
    }
    else if(i<100){
      idTxt <- paste(c('0',i), collapse = "")
    } 
    else {idTxt <- i}
    
    ##print(paste(c(directory,'/', idTxt, '.csv'), collapse = ""))
    
    
    ## load current file
    currFile <-  read.csv(paste(c(directory,'/', idTxt, '.csv'), collapse = ""))
    ##print(currFile)
    nobs<-sum(complete.cases(currFile))
    #stacktomainfile - if data frame not yet created, create it
    if(nobs>=threshold){
      if(exists("myVector")){
        myVector<-c(myVector, cor(currFile$sulfate, currFile$nitrate, use="complete.obs"))
      }
      else{
        myVector<-cor(currFile$sulfate, currFile$nitrate, use="complete.obs")
      }
    }
    
  } ##exit loop 
  myVector
}

justtheloop<-function(id=1:332){
  for(i in id){ 
    print(i)
  }
  
}
