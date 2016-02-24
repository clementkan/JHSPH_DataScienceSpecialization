## Solution 1 of 2:
pollutantmean <- function(directory, pollutant, id = 1:332) {
        files <- list.files(directory, full.names=TRUE)
                
        data <- data.frame()
        for(i in id){
                data <- rbind(data, read.csv(files[i]))
        }
        
        mean(data[,pollutant], na.rm=TRUE)
}

## Solution 2 of 2:
pollutantmean <- function(directory, pollutant, id = 1:332) {
        files <- list.files(directory, full.names=TRUE)
                
        data <- data.frame()
        for(i in id){
                data <- rbind(data, read.csv(files[i]))
        }
        
        if(pollutant == "sulfate"){
                subset <- data[,"sulfate"]
        } else {
                subset <- data[,"nitrate"]
        }
        
        mean(subset, na.rm = TRUE)
}
