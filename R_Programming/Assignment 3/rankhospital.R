rankhospital <- function(state, outcome, num = "best"){
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
        
        ## Return hospital name in that state with the given rank 30-day death rate
        data3 <- data2[data2[,"State"] == state, c("hospital name", outcome)]
        data3[,2] <- as.numeric(data3[,2])
        data3 <- data3[order(data3[,2],data3[,1]),]
        
        good <- complete.cases(data3)
        data4 <- data3[good,]

        if(num == "best"){
                return(data4[1,1])
        }
        if(num == "worst"){
                return(data4[nrow(data4),1])
        }
        if(num %in% 1:nrow(data4)){
                return(data4[num,1])
        } else {
                return(NA)
        }
}