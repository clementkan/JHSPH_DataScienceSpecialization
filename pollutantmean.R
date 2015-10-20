Solution 1 of 2:
pollutantmean <- function(directory, pollutant, id = 1:332){
        files<-list.files(directory, full.names=T)
        
        dat<-data.frame()
        for(i in id){
                dat<-rbind(dat, read.csv(files[i]))
        }
        
        subset<- dat[,pollutant]
        mean(subset, na.rm=T)
}

Solution 2 of 2:
pollutantmean <-function(directory, pollutant, id = 1:332){
        files_list <- list.files(directory, full.names = TRUE)
        dat <- data.frame()
        for(i in id){
                dat <- rbind(dat, read.csv(files_list[i]))
        }
        if(pollutant == "sulfate"){
                dat_subset <- dat[,"sulfate"]
        } else{
                dat_subset <- dat[,"nitrate"]
        }
        mean(dat_subset, na.rm = TRUE)
}
