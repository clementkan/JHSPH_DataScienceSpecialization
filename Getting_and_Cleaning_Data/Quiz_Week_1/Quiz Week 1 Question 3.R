if(!file.exists("Natural Gas Aquisition Program.xlsx")){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
        download.file(fileUrl, destfile = "Natural Gas Aquisition Program.xlsx")
}

library(xlsx)

colIndex <- 7:15
rowIndex <- 18:23

dat <- read.xlsx("Natural Gas Aquisition Program.xlsx", sheetIndex = 1, colIndex = colIndex, rowIndex = rowIndex)

sum(dat$Zip*dat$Ext,na.rm=T)