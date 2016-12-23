library(XML)

fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata/data/restaurants.xml"
doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
rootNode <- xmlRoot(doc)

zipcode <- xpathSApply(rootNode,"//zipcode", xmlValue)

length(zipcode[zipcode==21231])