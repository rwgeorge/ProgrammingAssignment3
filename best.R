## best.R
##
## Bob George  https://github.com/rwgeorge
###############################################################################
## Coursera R Programming (rprog-011)
## Assignment 3: Hospital Quality
## https://github.com/rwgeorge/ProgrammingAssignment3
###############################################################################

## This function reads the outcome-of-care-measures.csv file and returns a 
## character vector with the name of the hospital that has the best 
## (i.e. lowest) 30-day mortality for the specified outcome in that state.
## The hospital name is the name provided in the Hospital.Name variable.
## The outcomes can be one of “heart attack”, “heart failure”, 
## or “pneumonia”. Hospitals that do not have data on a particular outcome
## should be excluded from the set of hospitals when deciding the rankings.
best <- function(state, outcome) {
    ## Check supplied outcome.
    outcomes <- c("heart attack", "heart failure", "pneumonia") 
    if (!(outcome %in% outcomes)) {
        stop("invalid outcome")
    }
    
    ## Get and order the data.
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check supplied state.
    if (!(state %in% data$State)) {
        stop("invalid state")
    }

    ## Get state data.
    stateData <- subset(data, State == state)
    
    ## Get column number.
    if (outcome == "heart attack")
        outcomeColumn <- 11
    else if (outcome == "heart failure")
        outcomeColumn <- 17
    else
        outcomeColumn <- 23    

    outcomeColumnData <- as.numeric(stateData[,outcomeColumn])
    row <- which.min(outcomeColumnData)
    stateData$Hospital.Name[row]
}