corr <- function(directory, threshold = 0){
        files_list <- list.files(directory, full.names = TRUE)
        dat <- vector()
        for(i in 1:332){
                read_file <- read.csv(files_list[i])
                good <- complete.cases(read_file)
                y <- read_file[good,]
                nob <- nrow(y)
                if(nob > threshold){
                        x <- cor(read_file$nitrate, read_file$sulfate, use ="complete.obs")
                        dat <- rbind(dat, x)
                }
        }
        z <- as.numeric(dat)
        print(z)
}
