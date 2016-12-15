rankall <- function(outcome, num = "best"){
        ##Read outcome data
        data1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ##Check that outcome is valid
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop("invalid outcome")
        }
        
        ##For each state, find the hospital of the given rank
        switch(outcome, 
               "heart attack" = {col <- 11},
               "heart failure" = {col <- 17},
               "pneumonia" = {col <- 23}
        )
        
        data2 <- data1[,c(2, 7, col)]
        ## colnames(data2) <- c("hospital name", "state", outcome)
        
        data2[data2[,3] == "Not Available",] <- NA
        data3 <- na.omit(data2)
        data3[,3] <- as.numeric(data3[,3])
        data3 <- data3[order(data3[,2], data3[,3], data3[,1]),]
        
        unique_state <- unique(data3[,2])
        output <- data.frame()
        
        for(i in seq(unique_state)){
                x <- data3[data3[,2] == unique_state[i],]
                
                if(num == "best"){result <- x[1,1]}
                else if(num == "worst"){result <- x[nrow(x),1]}
                else if(num %in% 1:nrow(x)){result <- x[num,1]}
                else if(num > nrow(x)){result <- NA}
                
                y <- cbind(result, unique_state[i])
                output <- rbind(output, y)
                
        }
        
        output <- output[order(output[,2]),]
        rownames(output) <- output[,2]
        colnames(output) <- c("hospital", "state")
        data.frame(output)
}