library(devtools)
install_github("genomicsclass/GSE5859Subset")

library(GSE5859Subset)

data(GSE5859Subset) ##this loads the three tables

head(sampleInfo)

## Exercise 1 - - Week 1

sum(sampleInfo$date == as.Date("2005-06-27"))


## Excercise 2


dim(geneAnnotation)  # dimensions
dim(geneExpression)
dim(sampleInfo)

head(geneAnnotation)
head(geneExpression)


unique(geneAnnotation$CHR)  # we look at the different chromosomes

library(dplyr)

sum(geneAnnotation$CHR == "chrY", na.rm = T)  # how many genes are on
                                              # are on chromosome Y?

## Excercise 3

# What is the log expression value of the for gene ARPC1A on the one subject
# that we measured on 2005-06-10?


rownames(geneExpression[1:3, ])

id.sel <- geneAnnotation[!is.na(geneAnnotation$SYMBOL) & geneAnnotation$SYMBOL == "ARPC1A",
                         "PROBEID"]  # manufacturer ID

subject.id <- sampleInfo[sampleInfo$date == as.Date("2005-06-10"),
                         "filename"]  # File name

geneExpression[id.sel, subject.id]  # Answer



## Excercise 4

# Use the function apply to find the median value of each column.
# What is the median value of these values?

x.medians <- apply(geneExpression, 2, median)
median(x.medians)




## Excercise 5

GetTTest <- function(e, group){
  return(t.test(e[group == 1], e[group == 0])$p.value)
}

g <- factor(sampleInfo$group)  # Encode

# suppose the order of the samples is identical to the encoding
min(apply(geneExpression, 1, GetTTest, g))  # minimum

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


### Decompose the Time Serie

f <- decompose(AirPassengers)
plot(f$figure, type = "c")
plot(f$seasonal, type = "l")
plot(f$trend, type = "l")


## Advent of Code

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


# Inference in Practice Exercises

## Exercise 1

set.seed(1)
library(downloader)
url = "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename = "femaleControlsPopulation.csv"
if (!file.exists(filename)) download(url,destfile=filename)
population = read.csv(filename)
pvals <- replicate(1000,{
  control = sample(population[,1],12)
  treatment = sample(population[,1],12)
  t.test(treatment,control)$p.val
})
head(pvals)
hist(pvals)

### answer
sum(pvals < 0.05) / length(pvals)

## Exercise 2

sum(pvals < 0.01) / length(pvals)




## Exercise 3

cases = rnorm(10,30,2)
controls = rnorm(10,30,2)
t.test(cases,controls)

set.seed(100)

pvals <- replicate(20, {
  cases = rnorm(10, 30, 2)
  controls = rnorm(10, 30, 2)
  t.test(cases, controls)$p.value
})

sum(pvals < 0.05)


## Exercise 4

set.seed(100)

pvals.rejected <- replicate(1000, {
  pvals <- replicate(20, {
    cases = rnorm(10, 30, 2)
    controls = rnorm(10, 30, 2)
    t.test(cases, controls)$p.value
  })
  
  sum(pvals < 0.05)
})

mean(pvals.rejected)

## Exercise 5 (tricky by the "at least once")

sum(pvals.rejected >= 1) / length(pvals.rejected)
