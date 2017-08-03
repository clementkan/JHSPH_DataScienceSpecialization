corr <- function(directory, threshold = 0){
        files_full <- list.files(directory, full.names = TRUE)
        
        data <- numeric()
        for(i in seq_along(files_full)){
                x <- read.csv(files_full[i])
                y <- x[complete.cases(x), ]
                
                if(nrow(y) > threshold){
                        cr <- cor(y$sulfate, y$nitrate)
                        data <- c(data, cr)
                }
        }
        return(data)
}
