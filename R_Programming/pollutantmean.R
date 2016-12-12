## R Programming Assignment 1 Answers - week 2: Air Pollution

## Question 1: pollutantmean.R
## Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA.

pollutantmean <- function(directory, pollutant, id = 1:332)

## Solution 1 of 2:
## Fastest solution by create an empty list (tmp) that’s the length of the expected output.
## Refer to R programming practice assignment - week 2 for more information. 

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
## Slower than solution 1 because the data frame is recopied at each loop. 

pollutantmean <- function(directory, pollutant, id = 1:332) {
        files <- list.files(directory, full.names = TRUE)
        
        data <- data.frame()
        
        for(i in id){
                data <- rbind(data, read.csv(files[i]))
        }
        
        subset <- data[,pollutant]
        mean(subset, na.rm = TRUE)
}
