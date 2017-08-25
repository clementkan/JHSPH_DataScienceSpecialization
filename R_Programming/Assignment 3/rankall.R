# Solution 1 of 2:
# This solution is easier for beginner to understand the code. The approach for building the 
# data frame is suboptimal, its build the dataframe by coping and re-copying them inside of 
# the loop. Solution 2 is a better approach to create a dataframe.
rankall <- function(outcome, num = "best"){
        
        ## Read outcome data
        data1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        switch(outcome,
               "heart attack" = {col <- 11},
               "heart failure" = {col <- 17},
               "pneumonia" = {col <- 23}
        )
        
        data2 <- data1[, c(2, 7, col)]
        names(data2) <- c("hospital", "state", outcome)
        
        ## Check that outcome is valid
        if(!outcome == names(data2[3])){
                stop("invalid outcome")
        }
        
        ## Ranking hospitals in all states
        data2[data2[,3] == "Not Available",] <- NA
        data3 <- na.omit(data2)
        data3[,3] <- as.numeric(data3[,3])
        data3 <-data3[order(data3[,2], data3[,3], data3[,1]),] #order by state, outcome, hospital
        
        unique_state <- unique(data3$state)
        
        output <- data.frame()
        
        for(i in seq(unique_state)){
                x <- data3[data3$state == unique_state[i],]
                
                if(num == "best"){result <- x[1,1]}
                else if(num == "worst"){result <- x[nrow(x),1]}
                else if(num %in% 1:nrow(x)){result <- x[num,1]}
                else if(num > nrow(x)){result <- NA}
                
                y <- cbind(result, unique_state[i])
                output <- rbind(output, y)
        }
        
        names(output) <- c("hospital","state")
        row.names(output) <- unique_state
        
        return(output)
}

# Solution 2 of 2:
# This solution is a better approach to create a dataframe.
rankall <- function(outcome, num = "best"){
        
        ## Read outcome data
        data1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        switch(outcome, 
               "heart attack" = {col <- 11},
               "heart failure" = {col <- 17},
               "pneumonia" = {col <- 23}
        )
        
        data2 <- data1[, c(2, 7, col)]
        names(data2) <- c("hospital", "state", outcome)
        
        ## Check that outcome is valid
        if(!outcome == names(data2[3])){
                stop("invalid outcome")
        }
        
        ## Ranking hospitals in all states
        data2[data2[,3] == "Not Available",] <- NA
        data3 <- na.omit(data2)
        data3[,3] <- as.numeric(data3[,3])
        data3 <-data3[order(data3[,2], data3[,3], data3[,1]),] #order by state, outcome, hospital
        
        unique_state <- unique(data3$state)
        
        tmp <- vector(mode = "list", length = length(unique_state))
        
        for(i in seq(unique_state)){
                x <- data3[data3$state == unique_state[i],]
                
                if(num == "best"){result <- x[1,1]}
                else if(num == "worst"){result <- x[nrow(x),1]}
                else if(num %in% 1:nrow(x)){result <- x[num,1]}
                else if(num > nrow(x)){result <- NA}
                
                tmp[[i]] <- c(result, unique_state[i])
        }
        output <- do.call(rbind,tmp)
        output <- data.frame(output) #convert ouput from matrix to data.frame
        
        names(output) <- c("hospital","state")
        row.names(output) <- unique_state
        
        return(output)
}
