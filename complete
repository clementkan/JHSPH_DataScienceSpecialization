Version 1:
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

Version 2:
complete <- function(directory, id = 1:332){
        files_list <- list.files(directory, full.names = TRUE)
        files_read <- data.frame()
        nobs <- data.frame()
        for(i in id){
                files_read <- read.csv(files_list[i])
                good <- complete.cases(files_read)
                x <- files_read[good,]
                nobs <- rbind(nobs, nrow(x))
                }
        y <- cbind(id, nobs)
        colnames(y)<-c("id","nobs")
        print(y)
}
