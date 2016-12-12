## R Programming Assignment 1 Answers - week 2: Air Pollution

## Question 2: pollutantmean.R

## Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. 
## The function should return a data frame where the first column is the name of the file and the second column is the number 
## of complete cases.

complete <- function(directory, id = 1:332)

## Solution 1 of 2:

complete <- function(directory, id = 1:332) {
        files <- list.files(directory, full.names = TRUE)
        
        data <- data.frame()
        for(i in id){
                x <- read.csv(files[i])
                good <- complete.cases(x)
                
                y <- c(i, nrow(x[good,]))
                
                data <- rbind(data, y)
        }
        colnames(data) <- c("id", "nobs")
        return(data)
}

## Solution 2 of 2:
## Solution from other people.

complete <- function(directory, id = 1:332){
        files_list <- list.files(directory, full.names = TRUE)
        
        dat <- data.frame()
        for(i in id){
                dat <- rbind(dat,read.csv(files_list[i]))
        }
        
        good <- complete.cases(dat)
        good_subset <- dat[good,][,"ID"]
        x <- as.data.frame(table(good_subset))
        colnames(x)<-c("id","nobs")
        target <- id
        y <- x[match(target,x$id),]
        print(y)
}
