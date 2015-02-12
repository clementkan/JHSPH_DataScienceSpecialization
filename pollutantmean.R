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
