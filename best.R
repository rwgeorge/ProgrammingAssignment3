## best.R
##
## Bob George  https://github.com/rwgeorge
###############################################################################
## Coursera R Programming (rprog-011)
## Assignment 3: Hospital Quality
## https://github.com/rwgeorge/ProgrammingAssignment3
###############################################################################

best <- function(state, outcome) {
    ## Check supplied outcome.
    outcomes <- c("heart attack", "heart failure", "pneumonia") 
    if (!(outcome %in% outcomes)) {
        stop("Invalid outcome!  Must be \"heart attack\", \"heart failure\", or \"pneumonia\".")
    }
    
    ## Get and order the data.
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check supplied state.
    if (!(state %in% data$State)) {
        stop("Invalid state!")
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