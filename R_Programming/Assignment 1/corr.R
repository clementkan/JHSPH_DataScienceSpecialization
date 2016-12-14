corr <- function(directory, threshold = 0){
        files_full <- list.files(directory, full.names = TRUE)
        
        output <- numeric()
        for(i in seq_along(files_full)){
                data1 <- read.csv(files_full[i])
                data2 <- na.omit(data1)
                data3 <- nrow(na.omit(data1))
               
                if(data3 > threshold){
                        cr <- cor(data2$sulfate, data2$nitrate)
                        output <- c(output, cr)
                }
        }
        
        return(round(output, 5))
            
}
