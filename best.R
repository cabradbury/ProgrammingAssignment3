## R Programming Assignment 3
## best()
##
## This function will accept two arguments:
##
## 1. 2-character abbreviated name of the a state
## 2. Outcome name
##
## This will read in the outcome-of-care-measures.csv file and return a character
## vector with the name of the hospital that has the best (i.e. lowest) 30-day
## mortality for the specified outcome in that state. The outcomes can be on of 
## "heart attack", "heart failure", or "pneumonia". Hospitals that do not have 
## data on a particular outcome should be excluded when determining rankings. 
##
## The fucntion sould validate the input - If any invalid state or outcome is 
## passed in, the function should throw an error via the "stop" function with 
## the exact message "invalid state" or "invalid outcome". 

best <- function(state, outcome) {
        ## Read outcome data
        dataInput <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available", header = TRUE)
        
        ## Check that state and outcome are valid
        
        validOutcome = c("heart attack", "heart failure", "pneumonia")
        if(!outcome %in% validOutcome) { stop("invalid outcome") }
        
        validState = unique(dataInput$State)
        if(!state %in% validState) stop("invalid state")
        
        
        ## Return hospital name in that state with the lowest 30-day death rate
        fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                         "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                         "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        colName <- fullColName[match(outcome,validOutcome)]
        
        dataInput.state <- dataInput[dataInput$State==state,]
        index <- which.min(as.double(dataInput.state[,colName]))
        dataInput.state[index,"Hospital.Name"]
}