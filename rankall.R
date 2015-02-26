## R Programming Assignment 3
##
## rankall()
##
## This function will take two arguments:
##
## 1. An outcome
## 2. A hospital ranking (num)
##
## The function reads in the outcome-of-care-measures.csv file and returns a
## two column data frame containing the hospital in each state that has the
## ranking specified in num. 
##
## Example:
##
## rankall("heart attack", "best")
##
## This will return a data frame containing the names of the hospitals that 
## are the best in their respective staets for 30-day heart attack deaths 
## rates. 
##
## The function should return a value for every state (some may be NA). The 
## first column of the data frame is named "hospital", which is the hospital
## name and the second column is named state, which is the 2-character 
## abbreviated state name. Hospitals that do not have a particular outcome 
## should be excluded from the list. 
##
## Ties should be handled in the same way the rankall function handled them. 

rankall <- function (outcome, num = "best") {
        
        # Read the outcome data in. 
        dataInput <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available", header = TRUE)
        
        # Check that the state and the outcome are valid.
        validOutcome = c("heart attack", "heart failure", "pneumonia")
        if (!outcome %in% validOutcome) { stop("invalid outcome") }
        
        validState = sort(unique(dataInput[,7]))
        
        # Convert the outcome name into the column name (makes the rest of the code
        # easier to deal with). This was implemented on the best.R code as part 
        # of the last piece of code. 
        fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                         "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                         "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        colName <- fullColName[match(outcome,validOutcome)]
        
        # For each state, find the hospital of the given rank.
        hospital <- character(0)
        
        for (i in seq_along(validState)) {
                # Return hospital name in that state with the given 30-day death rate.
                dataInput.state <- dataInput[dataInput$State==validState[i],]
                
                # Order data by outcome.
                dataInput.state.sorted <- dataInput.state[order(as.numeric(dataInput.state[[colName]]),
                                                                dataInput.state[["Hospital.Name"]],
                                                                decreasing = FALSE, na.last = NA), ]
                
                # Handle "num" input for best and worst. Since we want to handle the instance
                # of num passed in this for loop, we use the "this." function to focus on this
                # instance of num. 
                this.num = num
                if (this.num == "best") this.num = 1
                if (this.num == "worst") this.num = nrow(dataInput.state.sorted)
                
                hospital[i] <- dataInput.state.sorted[this.num, "Hospital.Name"]
        }
        
        # Return a data frame with the hospital names and the 
        # abbreviated state name. 
        
        data.frame(hospital = hospital, state = validState, row.names = validState)
        
}