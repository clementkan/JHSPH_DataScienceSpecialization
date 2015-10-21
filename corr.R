Solution 1 of 2:
## Two functions in a script {complete() and corr()}
corr <- function(directory, threshold = 0) {
        files<-list.files(directory, full.names=T)
        
        dat<-vector()
        for(i in 1:332){
                x<-read.csv(files[i])
                good<-complete.cases(x)
                y<-x[good,]
                z<-nrow(y)
                
                if(z > threshold){
                        c <- cor(sulfate, nitrate, use ="complete.obs")
                        dat <- c(dat, c)
                }
                
        }
        print(z)        
}

Solution 2 of 2:
## complete() was called in the corr()
corr <- function(directory, threshold = 0){
        files<-list.files(directory, full.names=T)
        
        dat<-vector()
        x<-complete()
        if(x[,"nobs"] > threshold){
                c <- cor(sulfate, nitrate, use ="complete.obs")
                dat <- c(dat, c)
        }
        print(dat)
}
