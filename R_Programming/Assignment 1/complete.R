complete <- function(directory, id = 1:332){
        files_full <- list.files(directory, full.names = TRUE)
        
        data <- data.frame()
        for(i in id){
                x <- read.csv(files_full[i])
                y <- x[complete.cases(x), ]
                z <- c(i, nrow(y)) 
                
                data <- rbind(data, z)
        }
        
        names(data) <- c("id", "nobs")
        data
}
