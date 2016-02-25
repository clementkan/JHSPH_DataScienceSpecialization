Solution 1 of 2:
## Two functions in a script {complete() and corr()}
corr <- function(directory, threshold = 0){
        files <- list.files(directory, full.names = TRUE)
        
        data <- numeric()
        for(i in 1:length(files)){
                x <- read.csv(files[i])
                good <- complete.cases(x)
                y <- x[good,]
                no.cases <- nrow(y)
                if(no.cases > threshold){
                        cr <- cor(y$sulfate, y$nitrate)
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
