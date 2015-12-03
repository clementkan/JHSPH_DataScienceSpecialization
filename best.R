R Programming - Assignment 3
Question 2: Finding the best hospital in a state

Script test:
source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")

Solution 1 of 2:
best <- function(state, outcome){
        
## Read the outcome data
        readfile <- read.csv("outcome-of-care-measures.csv")

## 1) Dataset contains a lot of variables, to keep it manageable, we only extract those variables
## that are required. 2) Check that the outcome are valid (heart attack, heart failure, pneumonia),
## if invalid the function will throw an error via the stop function with message "invalid outcome".
        if(outcome == "heart attack"){
                data1 <- readfile[, c(2, 7, 11)] 
        } else if (outcome == "heart failure"){
                data1 <- readfile[, c(2, 7, 17)]
        } else if (outcome == "pneumonia"){
                data1 <- readfile[, c(2, 7, 23)]
        } else stop("invalid outcome")

## Extract the state (argument) that we're interested.
        data2 <- data1[which(data1[,2] == state),]
        
## Check that the state arugment is valid, if not the function will throw an error via the stop 
## function with message "invalid state".
        if(nrow(data2) == 0){
                stop("invalid state")
        }
        
        names(data2) <- c("hospital name", "state", outcome)

## All three variables are class factors; we need to convert the outcome variable to numeric.
        data3 <- data2[, c(1, 2)]
        data4 <- as.numeric(as.character(data2[, 3]))
        data5 <- data.frame(data3, data4)
 
## Order (sort) the dataframe by outcome and hospital name.
        data <- data5[order(data5[,3], data5[,1]),]

## Return the hospital name (character vector) 
        return(data[1, 1])
}

Solution 2 of 2:
best <- function(state, outcome) {
        readfile <- read.csv("outcome-of-care-measures.csv", colClasses="character")
        
        column <- if (outcome == "heart attack") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "heart failure") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if (outcome == "pneumonia") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
                stop("invalid outcome")
        }
        
        data <- readfile[readfile$State == state, c("Hospital.Name", column)]
        
        if (nrow(data) == 0) {
                stop("invalid state")        
        }
        
        data[,2] <- as.numeric(data[,2])
        
        ordered_data <- order(data[column], data$Hospital.Name)
        
        as.character(data$Hospital.Name[ordered_data[1]])
}
