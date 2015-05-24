corr <- function(directory, threshold = 0)
    {
    files <- list.files(directory, full.names = TRUE);
    correllations <- numeric(0);

    for(i in files)
        {
        data <- read.csv(i);
        complete <- na.omit(data);

        if(threshold < nrow(complete))
            {
            correllations <- c(correllations, cor(complete$sulfate, complete$nitrate))
            }
        }

    return(correllations)
    }
