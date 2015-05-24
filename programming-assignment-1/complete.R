complete <- function(directory, id=1:332)
    {
    count <- length(id);
    complete_data <- numeric(0);

    if (count > 0)
        {
        files <- list.files(directory, full.names = TRUE);
        complete_data <- data.frame(id = numeric(0), nobs=numeric(0));

        for(i in id)
            {
            data <- read.csv(files[i]);
            comp <- na.omit(data);
            comp_entry <- c(i, nrow(comp));
            complete_data <- rbind(complete_data, comp_entry);
            }

        colnames(complete_data) <- c("id","nobs");
        }

    return(complete_data)
    }
