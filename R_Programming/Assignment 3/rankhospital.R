rankhospital <- function(state, outcome, num = "best"){
        
        ## Read outcome data
        data1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        switch(outcome, 
               "heart attack" = {col <- 11},
               "heart failure" = {col <- 17},
               "pneumonia" = {col <- 23}
        )
        
        data2 <- data1[, c(2, 7, col)]
        names(data2) <- c("Hospital.name", "State", outcome)
        
        ## Check that state and outcome are valid
        if(!state %in% data2$State){
                stop("invalid state")
        }
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop("invalid outcome")
        }
        
        ## Ranking hospitals by outcome in a state
        data3 <- data2[data2$State == state, c("Hospital.name", outcome)]
        
        data3[data3[,2] == "Not Available",] <- NA
        data4 <- na.omit(data3)
        data4[,2] <- as.numeric(data4[,2])
        
        data5 <- data4[order(data4[,2], data4[,1]),]
        
        if(num == "best"){
                return(data5[1,1])
        } else if(num == "worst"){
                return(data5[nrow(data5),1])
        } else if(num %in% 1:nrow(data5)){
                return(data5[num,1])
        } else{
                return(NA)
        }
}
