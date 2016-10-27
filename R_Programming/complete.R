## R Programming (week 2): Assignment 1, Air Pollution
## Solution 1 of 3:
complete <- function(directory, id = 1:332){
        files <- list.files(directory, full.names = TRUE)
        
        data <- data.frame()
        for(i in id){
                x <- read.csv(files[i])
                good <- complete.cases(x)
                nobs <- c(i, nrow(x[good,]))
                data <- rbind(data, nobs)
        }
        
        names(data) <- c("id", "nobs")
        return(data)
}
## Solution 2 of 3:
## Works, but not a good solution.
complete <- function(directory, id = 1:332){
        files <- list.files(directory, full.names = TRUE)
        
        dat <- data.frame()
        nobs <- data.frame()
        
        for(i in id){
                dat <- read.csv(files[i])
                good <- complete.cases(dat)
                x <- dat[good,]
                nobs <- rbind(nobs, nrow(x))
        }
        
        y <- cbind(id, nobs)
        names(y) <- c("id","nobs")
        print(y)
}

## Solution 3 of 3:
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
