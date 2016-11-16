library(devtools)
install_github("genomicsclass/GSE5859Subset")

library(GSE5859Subset)
data(GSE5859Subset) ##this loads the three tables

head(sampleInfo)

sum(sampleInfo$date == as.Date("2005-06-27"))
