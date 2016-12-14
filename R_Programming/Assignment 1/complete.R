complete <- function(directory, id = 1:332){
        files_full <- list.files(directory, full.names = TRUE)
        
        output <- data.frame()
        for(i in id){
                data1 <- read.csv(files_full[i])
                data2 <- na.omit(data1)
                data3 <- cbind(i, nrow(data2))
                
                output <- rbind(output, data3)
        }
        
        colnames(output) <- c("id", "nobs")
        rownames(output) <- seq_along(id)
        
        return(output)
}
