## R Programming (week 2): Assignment 1, Air Pollution
## Solution 1 of 1:
## This script checks if the number of complete cases > threshold given,
## if "yes", its calculates the correlation between sulfate and nitrate for the monitor.
## Returns a numeric vector containing the correlations.
corr <- function(directory, threshold = 0) {
        files <- list.files(directory, full.names = TRUE)
        
        data <- numeric()
        for(i in 1:length(files)){
                x <- read.csv(files[i])
                good <- complete.cases(x)
                y <- x[good,]
                complete_cases <- nrow(y)
             
                if(complete_cases > threshold){
                        cr <- cor(y$sulfate, y$nitrate)
                        data <- c(data, cr)
                }   
        }
        return(data)
}
