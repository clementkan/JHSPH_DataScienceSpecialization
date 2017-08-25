best <- function(state, outcome){
        
        ## Read outcome data
        data1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        data2 <- data1[, c(2, 7, 11, 17, 23)]
        names(data2) <- c("Hospital.name", "State", "heart attack", "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
        if(!state %in% data2$State){
                stop("invalid state")
        }
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop("invalid outcome")
        }
        
        ## Return the best hospital in that state
        data3 <- data2[data2$State == state, c("Hospital.name", outcome)]
        
        data3[data3[,2] == "Not Available",] <- NA
        data4 <- na.omit(data3)
        data4[,2] <- as.numeric(data4[,2])
        
        data5 <- data4[order(data4[,2], data4[,1]),]
        return(data5[1,1])
        
}
