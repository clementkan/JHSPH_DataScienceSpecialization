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