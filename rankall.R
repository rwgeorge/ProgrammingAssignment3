## rankall.R
##
## Bob George  https://github.com/rwgeorge
###############################################################################
## Coursera R Programming (rprog-011)
## Assignment 3: Hospital Quality
## https://github.com/rwgeorge/ProgrammingAssignment3
###############################################################################

## This function reads the outcome-of-care-measures.csv file and returns a 
## 2-column data frame containing the hospital in each state that has the 
## ranking specified in num. For example the function call 
## rankall("heart attack", "best") would return a data frame containing the 
## names of the hospitals that are the best in their respective states for 
## 30-day heart attack death rates. The function should return a value for 
## every state (some may be NA). The first column in the data frame is named
## hospital, which contains the hospital name, and the second column is named 
## state, which contains the 2-character abbreviation for the state name. 
## Hospitals that do not have data on a particular outcome should be excluded 
## from the set of hospitals when deciding the rankings.
rankall <- function(outcome, num = "best") {
    ## Check supplied outcome.
    outcomes <- c("heart attack", "heart failure", "pneumonia") 
    if (!(outcome %in% outcomes)) {
        stop("invalid outcome")
    }
    
    ## Get the data.
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    dataByHospital <- data[order(data$Hospital.Name),]
    
    ## Get column number.
    if (outcome == "heart attack")
        outcomeColumn <- 11
    else if (outcome == "heart failure")
        outcomeColumn <- 17
    else
        outcomeColumn <- 23
    
    #result <- data.frame()
    #for (item in split(dataByHospital, dataByHospital$State)) {
    #    result <- rbind(result, findRow(item, outcomeColumn, num))
    #}

    findRow <- function(data) {
        column <- as.numeric(data[,outcomeColumn])
        if (num=="best")
            row <- which.min(column)
        else if (num=="worst")
            row <- which.max(column)
        else {
            data <- data[order(column),]
            row <- num
        }
        
        c(data$Hospital.Name[row], data[,7][row])
    }
    
    appliedRows <- lapply(split(dataByHospital, dataByHospital$State), findRow)
    result <- data.frame(matrix(unlist(appliedRows), ncol=2, byrow=T))
    
    colnames(result) <- c('hospital','state')
    result[order(result$state),]
}