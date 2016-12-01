library(devtools)
install_github("genomicsclass/GSE5859Subset")

library(GSE5859Subset)

data(GSE5859Subset) ##this loads the three tables

head(sampleInfo)

## Exercise 1 - - Week 1

sum(sampleInfo$date == as.Date("2005-06-27"))

## Excercise 2

head(geneAnnotation)
head(geneExpression)


## Time series

data("AirPassengers")
class(AirPassengers)

start(AirPassengers)
cycle(AirPassengers)
frequency(AirPassengers)

plot(AirPassengers)

plot(aggregate(AirPassengers, FUN = mean))
ag <- aggregate(AirPassengers, FUN = mean)
ag


boxplot(AirPassengers~cycle(AirPassengers))
air.dfs <- diff(AirPassengers)
plot(diff(air.dfs))


acf(AirPassengers)
acf(diff(diff(AirPassengers)))  # better
pacf((AirPassengers))


dat <- read.table("input1_a.in", header = F, sep = ",", stringsAsFactors = F, strip.white = T)
lets <- sapply(1:ncol(dat), function(i) strsplit((dat[, i]), split = "")[[1]][1])
ints <- sapply(1:ncol(dat), function(i) strsplit((dat[, i]), split = "")[[1]][2])

lets <- strsplit((dat[1, 2]), split = "")[[1]][1]
class(dat$V1)


df <- data.frame("x" = lets, "y" = ints)
write.table(df, file = "entrada1_a.in", sep = " ", col.names = F, row.names = F)

ints <- as.numeric(ints)

tot.sum <- sum(ints[1:2])

myf <- function(j, arr, vals){
  
  current <- vals[j]
  
  if(arr[j] == arr[j-1])
    current <- -1*current
  
  current
}

res <- sapply(3:length(ints), myf, lets, ints)
