## R Programming (week 2): Assignment 1, Air Pollution
## Solution 1 of 2:
## Fastest solution, create an empty list (tmp) thats the length of the expected output.
## Refer to R programming, week 2, practice assignment for more information. 
pollutantmean <- function(directory, pollutant, id = 1:332){
        files <- list.files(directory, full.names = TRUE)
        
        tmp <- vector(mode = "list", length = length(id))
        for(i in id){
                tmp[[i]] <- read.csv(files[[i]])
        }
        data <- do.call(rbind, tmp) ## do.call() to combine tmp into a single data frame.
        
        mean(data[,pollutant], na.rm = TRUE)
}

## Solution 2 of 2:
## Slower as compare to solution 1, the data frame is recopy at each loop. 
pollutantmean <- function(directory, pollutant, id = 1:332) {
        files <- list.files(directory, full.names = TRUE)
        data <- data.frame()
        
        for(i in id){
                data <- rbind(data, read.csv(files[i]))
        }

        subset <- data[,pollutant]
        mean(subset, na.rm = TRUE)
}
