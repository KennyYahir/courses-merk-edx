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

