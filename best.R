## Determine the name of the hospital with the best outcome values in a given state.
## state - the state abreviation
## outcome - the outcome name.
best <- function(state, outcome)
    {
    ## Read outcome data
    Data <- read.csv("outcome-of-care-measures.csv", stringsAsFactor = FALSE);

    ## Check that state and outcome are valid
    if(!(state %in% Data$State))
        {
        stop("invalid state");
        }

    if(!(outcome %in% c("heart attack","heart failure","pneumonia")))
        {
        stop("invalid outcome");
        }

    ## Return hospital name in that state with lowest 30-day death rate
    column = switch(outcome, "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23);

    # 2 is hospital name, 7 is state
    values = Data[,c(2,7,column)];

    # removing all hospitals with no outcome data
    values = values[!(values[,3] == "Not Available"),];

    # Convert values to numerical values
    values[,3] = as.numeric(values[,3]);

    # excluding all rows from the wrong state
    values = values[values$State == state,];

    # get all rows that have the minimum value
    values = values[values[,3] == min(values[,3]),];

    # sorting by hospital name
    values = values[order(values[,2]),];

    # returning the hospital name from the first row
    return(values[1,1]);
    }
