## best.R
##
## Bob George  https://github.com/rwgeorge
###############################################################################
## Coursera R Programming (rprog-011)
## Assignment 3: Hospital Quality
## https://github.com/rwgeorge/ProgrammingAssignment3
## 
## Description: 
###############################################################################

best <- function(state, outcome) {
    ## Check supplied outcome.
    outcomes <- c("heart attack", "heart failure", "pneumonia") 
    if (!(outcome %in% outcomes)) {
        stop("Invalid outcome!  Must be \"heart attack\", \"heart failure\", or \"pneumonia\".")
    }
    
    ## Get and order the data.
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    byHospital <- data[order(data$Hospital.Name),]
    
    ## Check supplied state.
    states <- unique(byHospital$State)
    if (!(state %in% states)) {
        stop("Invalid state!")
    }

    ## Get state data.
    stateData <- subset(byHospital, State == state)
    outcomeData <- paste0("hospital.30.day.death..mortality..rates.from.", gsub(" ", ".", outcome))
    
    
}