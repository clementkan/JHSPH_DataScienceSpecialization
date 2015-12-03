## R Programming - Assignment 3
## Question 3: Ranking hospitals by outcome in a state

## Script test: source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")

rankhospital <- function(state, outcome, num = "best"){
        readfile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        file <- if(outcome == "heart attack"){
                readfile[readfile$State == state, c(2, 11)]
        } else if (outcome == "heart failure"){
                readfile[readfile$State == state, c(2, 17)]
        } else if (outcome == "pneumonia"){
                readfile[readfile$State == state, c(2, 23)]
        } else {
               stop("invalid outcome") 
        }
        
        if(nrow(file) == 0){
                stop("invalid state")
        }
        
        file[,2] <- as.numeric(file[,2])
        data1 <- file[order(file[,2], file[,1]),]
        data2 <- 1:nrow(data1)
        data <- cbind(data1, data2)
        
        good <- complete.cases(data)
        complete_data <- data[good,]
        names(complete_data) <- c("Hospital.Name", "Rate", "Rank")
        
        max_num <- nrow(complete_data)
                
        if(num == "best"){
                return(complete_data[1, 1])
        } else if (num == "worst"){
                return(complete_data[nrow(complete_data), 1])
        } else if (num <= max_num){
                return(complete_data[num, 1])
        } else {
                print(NA)
        }
         
}
