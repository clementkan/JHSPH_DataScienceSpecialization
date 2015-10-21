Solution 1:
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
