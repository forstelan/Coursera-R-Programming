pollutantmean <- function(directory, pollutant, id=1:332)
    {
    count <- length(id);
    collated_means <- 0;

    if (count > 0)
        {
        files <- list.files(directory, full.names = TRUE);
        collated_data <- read.csv(files[id[1]]);

        if (count > 1)
            {
            for(i in 2:length(id) )
                {
                data <- read.csv(files[id[i]]);
                collated_data <- rbind(collated_data, data);
                }
            }

        collated_means <- mean(collated_data[, pollutant], na.rm = TRUE);
        }

    return(collated_means);
    }
