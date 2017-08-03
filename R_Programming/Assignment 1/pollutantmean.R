# Solution 1 of 2: 
# This solution is a better approach to create a dataframe compared to solution 2. 
# Refer to R Programming Practice Assignment for more details on using this approach. 

pollutantmean <- function(directory, pollutant, id = 1:332){
        files_full <- list.files(directory, full.names = TRUE)
        
        tmp <- vector(mode = "list", length = length(id))
        for(i in id){
                tmp[[i]] <- read.csv(files_full[[i]])
        }
        
        data <- do.call(rbind, tmp)
        
        mean(data[, pollutant], na.rm = TRUE)  
}



# Solution 2 of 2:
# Requires longer computation time as compared to solution 1.
# The building of the data frame is suboptimal as the dataframe is recopied inside of loop.

pollutantmean <- function(directory, pollutant, id = 1:332){
        files_full <- list.files(directory, full.names = TRUE)
        
        data <- data.frame()
        for(i in id){
                data <- rbind(data, read.csv(files_full[i]))
        }
        
        mean(data[,pollutant], na.rm = TRUE)
}
