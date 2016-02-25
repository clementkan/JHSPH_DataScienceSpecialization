## Solution 1 of 2:
## This script checks if the number of completed cases > threshold given,
## if "yes", its calculates the correlation between sulfate and nitrate for the monitor.
## Returns a numeric vector containing the correlations.
corr <- function(directory, threshold = 0){
        files <- list.files(directory, full.names = TRUE)
        
        data <- numeric()
        for(i in 1:length(files)){
                x <- read.csv(files[i])
                good <- complete.cases(x)
                y <- x[good,]
                no.cases <- nrow(y)
                if(no.cases > threshold){
                        cr <- cor(y$sulfate, y$nitrate) ## alternative: cr <- cor(x$sulfate, x$nitrate, use = "complete.obs") 
                        data <- c(data, cr)
                }
        }
        return(data)
}

Solution 2 of 2:
## complete() was called in the corr()
corr <- function(directory, threshold = 0) {
        files<-list.files(directory, full.names=T)
        
        x<-complete()
        if(x[,"nobs"] > threshold){
                corr<-cor(nitrate, sulfate)
        }
        print(data)
}
