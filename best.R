## R Programming Assignment 3 - week 4
## Question 2: Finding the best hospital in a state
## Script test: source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")

## Solution 1 of 4:
best <- function(state, outcome){
  
## Read outcome data
  data1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data2 <- data1[,c(7,2,11,17,23)]
  colnames(data2) <- c("State", "hospital name","heart attack", "heart failure", "pneumonia")

## Check that state and outcome are valid  
  if(!state %in% data2$State){
          stop("invalid state")
  }
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
          stop("invalid outcome")
  }

## Return hospital name in that state with lowest 30-day death rate    
  data3 <- data2[data2[,"State"] == state, c("hospital name", outcome)]
  data3[,2] <- as.numeric(data3[,2])
  
  x <- data3[order(data3[,2], data3[,1]),]
  z <- x[1,1]
  return(z)
}

## Solution 2 of 4:
best <- function(state, outcome){
  data1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  if(outcome == "heart attack"){
    data2 <- data1[data1[,7] == state, c(2, 11)]
  } else if(outcome == "heart failure"){
    data2 <- data1[data1[,7] == state, c(2, 17)]
  } else if(outcome == "pneumonia"){
    data2 <- data1[data1[,7] == state, c(2, 23)]
  } else (stop("invalid outcome"))
  
  data2[,2] <- as.numeric(data2[,2])
  
  x <- data2[order(data2[,2], data2[,1]),]
  z <- x[1,1]
  
  if(!is.na(z)){
    print(z)
  } else {stop("invalid state")}
}

## Solution 3 of 4:
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

## Convert the outcome variable to numeric.
        data3 <- data2[, c(1, 2)]
        data4 <- as.numeric(as.character(data2[, 3]))
        data5 <- data.frame(data3, data4)
 
## Order (sort) the dataframe by outcome and hospital name.
        data <- data5[order(data5[,3], data5[,1]),]

## Return the hospital name (character vector) 
        return(data[1, 1])
}

## Solution 4 of 4:
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
