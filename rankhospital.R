## Rank hospitals in a given state according to a given outcome.
## state - the state abreviation
## outcome - the outcome name.
## num - the numerical ordering, best(default), worst, numerical value
rankhospital <- function(state, outcome, num = "best")
    {
    # Read outcome data
    Data <- read.csv("outcome-of-care-measures.csv", stringsAsFactor = FALSE);

    # Check that state is valid
    if(!(state %in% Data$State))
        {
        stop("invalid state");
        }

    # Check that outcome is valid
    if(!(outcome %in% c("heart attack","heart failure","pneumonia")))
        {
        stop("invalid outcome");
        }

    # Return hospital name in that state with lowest 30-day death rate
    column = switch(outcome, "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23);

    # 2 is hospital name, 7 is state
    values = Data[,c(2,7,column)];

    # removing all hospitals with no outcome data
    values = values[!(values[,3] == "Not Available"),];

    # Convert Characters to numerical values
    values[,3] = as.numeric(values[,3])

    # excluding all rows from the wrong state
    values = values[values$State == state,];

    # sort values according to mortalitry rates
    mortality = sort(values[,3]);

    # determine ranking method
    if(num == "best")
        {
        rank = 1;
        }
    else if(num == "worst")
        {
        rank = length(mortality);
        }
    else if(num > length(mortality))
        {
        return(NA);
        }
    else
        {
        rank = num;
        }

    # get all rows that have the minimum value
    values = values[values[,3] == mortality[rank],];

    # sorting by hospital name
    values = values[order(values[,2]),];

    # returning the hospital name from the first row
    return(values[1,1]);
    }
