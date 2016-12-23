if(!file.exists("2006 microdata survey Codebook.pdf")){
        url <- ("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf")
        download.file(url, "2006 microdata survey Codebook.pdf", mode="wb")
}

if(!file.exists("2006 microdata survey.csv")){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
        download.file(fileUrl, destfile = "2006 microdata survey.csv")
}

data <- read.csv("2006 microdata survey.csv")

print(table(data$VAL))