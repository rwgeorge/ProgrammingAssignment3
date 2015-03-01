## rankhospital.R
##
## Bob George  https://github.com/rwgeorge
###############################################################################
## Coursera R Programming (rprog-011)
## Assignment 3: Hospital Quality
## https://github.com/rwgeorge/ProgrammingAssignment3
###############################################################################

## The function reads the outcome-of-care-measures.csv file and returns a 
## character vector with the name of the hospital that has the ranking 
## specified by the num argument.  The num argument can take values “best”, 
## “worst”, or an integer indicating the ranking (smaller numbers are better).
## If the number given by num is larger than the number of hospitals in that
## state, then the function should return NA. Hospitals that do not have data 
## on a particular outcome should be excluded from the set of hospitals when 
## deciding the rankings.
rankhospital <- function(state, outcome, num = "best") {
    ## Check supplied outcome.
    outcomes <- c("heart attack", "heart failure", "pneumonia") 
    if (!(outcome %in% outcomes)) {
        stop("invalid outcome")
    }
    
    ## Get the data.
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check supplied state.
    if (!(state %in% data$State)) {
        stop("invalid state")
    }
    
    ## Get state data.
    stateData <- subset(data, State == state)
    
    ## Order by hospital name.
    byHospital <- stateData[order(stateData$Hospital.Name),]
    
    ## Get column number.
    if (outcome == "heart attack")
        outcomeColumn <- 11
    else if (outcome == "heart failure")
        outcomeColumn <- 17
    else
        outcomeColumn <- 23
    
    hospitalData <- as.numeric(byHospital[,outcomeColumn])
    
    if (num == "best")
        row <- which.min(hospitalData)
    else if (num == "worst")
        row <- which.max(hospitalData)
    else {
        byHospital <- byHospital[order(hospitalData),]
        row <- num  
    }
    
    byHospital$Hospital.Name[row]
}