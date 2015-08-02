best <- function(state, outcome) {
    
    data_set <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character",
                         na.strings="Not Available")
    
   
    good_data = c("heart attack","heart failure","pneumonia")
    if (!outcome %in% good_data) { stop("disease is invalid")}
    
    validState = unique(data_set[,7])
    if (!state %in% validState) stop("state us invalid")
    
    col_names <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    colName <- col_names[match(outcome,good_data)]
    
    data_set.state <- data_set[data_set$State==state,]
    idx <- which.min(as.double(data_set.state[,colName]))
    data_set.state[idx,"Hospital.Name"]
  }
