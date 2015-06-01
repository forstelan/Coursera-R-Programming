## Rank all hospitals in all states according to a given outcome.
## outcome - the outcome name.
## num - the numerical ordering, best(default), worst, numerical value
rankall <- function(outcome, num = "best")
    {
    # Read outcome data
    Data <- read.csv("outcome-of-care-measures.csv", stringsAsFactor = FALSE);

    # Check that outcome is valid
    if(!(outcome %in% c("heart attack","heart failure","pneumonia")))
        {
        stop("invalid outcome");
        }

    # Return hospital name in that state with lowest 30-day death rate
    column = switch(outcome, "heart attack" = 11, "heart failure" = 17, "pneumonia" = 23);

    # 2 is hospital name, 7 is state
    values = Data[,c(2,7,column)];

    #get list of states
    states = sort(unique(Data[,7]));

    # removing all hospitals with no outcome data
    values[(values[,3] == "Not Available"),3] = NA;

    # Convert Characters to numerical values
    values[,3] = as.numeric(values[,3]);

    # save a copy of the original values
    all_values = values;

    # construct empty data frame to populate by state
    ranked_values = data.frame();

    # rank in each state
    for(state in states)
        {
        # reset working copy to original values
        values = all_values;

        # excluding all rows from the wrong state
        values = values[values$State == state,];

        # sort values according to mortalitry rates
        mortality = sort(values[,3]);

        # determine ranking method
        if(num == "best"){
            rank = 1;
            }else if(num == "worst")
            {
            rank = length(mortality);
            }else if(num > length(mortality))
            {
            # fill NA values
            output = NA;
            ranked_values = rbind(ranked_values,
                                  data.frame(hospital = output,
                                            state = state));
            next;
            }else
            {
            rank = num;
            }

        # If any valid results remove invalid ones
        if(any(!is.na(values[,3]))){
            values = values[!is.na(values[,3]),]
        }else{# return invalid results
            output = NA;
            ranked_values = rbind(ranked_values,
                                     data.frame(hospital = output,
                                            state = state));
            next
        }

        # get all rows that have the ranked value
        values = values[values[,3] == mortality[rank],];

        # sorting by hospital name
        values = values[order(values[,1]),];

        # returning the hospital name from the first row
        ranked_values = rbind(ranked_values,
                             data.frame(hospital = values[1,1],
                                        state = state));
        }

    # return ranked data frame
    return(ranked_values);
    }
