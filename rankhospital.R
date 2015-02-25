## R Programming Assignment 3
##
## rankhospital()
##
## This function will take three arguments:
##
## 1. 2-character abbreviated state name. 
## 2. an outcome
## 3. ranking of the hospital in that state for that outcome (num). 
##
## The function reads the outcome-of-care-measures.csv file and returns
## a character vector with the name of the hospital that has the ranking
## specificied by the "num" argument. For example:
##
## rankhospital("MD", "heart failure", 5)
##
## This will return the name of the hospital with teh 5th lowest 30-day
## death rate for heart failure. 
##
## "num" can take values of "best", "works" or an integer indicating the 
## ranking. If the number given by "num" is larger than the number of 
## hospitals in that state, then the function should return NA. Hospitals
## that do not have data on a particular outcome should be excluded. 
##
## Ties should be handled by the name of the hospital in order
## using the order() function. 

rankhospital <- function(state, outcome, num = "best") {
        # Read outcome data
        dataInput <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available", header = TRUE)
        
        # Check that the state and outcomes are valid
        
        validOutcome = c("heart attack", "heart failure", "pneumonia")
        if(!outcome %in% validOutcome) { stop("invalid outcome") }
        
        validState = unique(dataInput$State)
        if(!state %in% validState) stop("invalid state")
        
        # Return the hospital name in that state with the given rank 30-day 
        # death rate. 
        
        # Convert the outcome name into the column name (makes the rest of the code
        # easier to deal with). This was implemented on the best.R code as part 
        # of the last piece of code. 
        fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                         "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                         "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        colName <- fullColName[match(outcome,validOutcome)]
        
        # Let's get hospital name in the specificed state that has the specified rank.
        # Also, order so we can determine rank for ties.
        
        dataInput.state <- dataInput[dataInput$State==state,]
        dataInput.state.sorted <- dataInput.state[order(as.numeric(dataInput.state[[colName]]),
                                                        dataInput.state[["Hospital.Name"]],
                                                        decreasing=FALSE, na.last=NA),]
        
        # Now, let's deal with how "num" is passed in, it can be "best", "worst" or a number.
        # The number will be automaticall passed in, since this is a numerical value, so the
        # only cases we need to handle are the best and worst, converting them to a numerical 
        # value. 
        
        if(num=="best") num = 1 # Assuming 1 is the lowest/best ranking.
        if(num == "worst") num = nrow(dataInput.state.sorted) # Set it to the row count / last row.
        
        # Return the hospital name of the state with the given rank. 
        dataInput.state.sorted[num, "Hospital.Name"]
        
}