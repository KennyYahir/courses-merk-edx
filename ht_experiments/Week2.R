
# Error Rates and Procedures Exercises


## Exercise 3

1 / 439.65

num.exp <- 8793
pval <- (1 / num.exp) * 0.05


set.seed(100)

pvals.rejected <- replicate(20, {
  pvals <- replicate(num.exp, {
    cases = rnorm(10, 30, 2)
    controls = rnorm(10, 30, 2)
    t.test(cases, controls)$p.value
  })
  
  sum(pvals < pval)
})

mean(pvals.rejected)

## Exercise 5 (tricky by the "at least once")

sum(pvals.rejected >= 1) / length(pvals.rejected)




# Vectorizing code

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"

filename <- "femaleControlsPopulation.csv"
if (!file.exists(filename)) 
  download(url,destfile=filename) 

set.seed(1) 
population = unlist( read.csv("femaleControlsPopulation.csv") )

alpha <- 0.05
N <- 12
m <- 10000
p0 <- 0.90 ##10% of diets work, 90% don't
m0 <- m*p0
m1 <- m-m0
nullHypothesis <- c( rep(TRUE,m0), rep(FALSE,m1))
delta <- 3

#sum(1:3 * c(TRUE, FALSE, TRUE))


B <- 10 ##number of simulations 
system.time(
  VandS <- replicate(B,{
    calls <- sapply(1:m, function(i){
      control <- sample(population,N)
      treatment <- sample(population,N)
      if(!nullHypothesis[i]) treatment <- treatment + delta
      t.test(treatment,control)$p.val < alpha
    })
    c(sum(nullHypothesis & calls),sum(!nullHypothesis & calls))
  })
)



## Second simulation

GetTTest <- function(e, group){
  return(t.test(e[group == 1], e[group == 0]))
}

MyRowtTests <- function(data, g){
  apply(data, 1, GetTTest, g)
}


source("https://bioconductor.org/biocLite.R")
biocLite("genefilter")

set.seed(1)
##Define groups to be used with rowttests
g <- factor( c(rep(0,N),rep(1,N)) )
B <- 10 ##number of simulations 

system.time(
  VandS <- replicate(B,{
    
    ##matrix with control data (rows are tests, columns are mice)
    controls <- matrix(sample(population, N*m, replace=TRUE),nrow=m)
    
    ##matrix with control data (rows are tests, columns are mice)
    treatments <-  matrix(sample(population, N*m, replace=TRUE),nrow=m)
    
    ##add effect to 10% of them
    treatments[which(!nullHypothesis),]<-treatments[which(!nullHypothesis),]+delta
    
    ##combine to form one matrix
    dat <- cbind(controls,treatments)
    
    #calls <- rowttests(dat,g)$p.value < alpha
    calls <- MyRowtTests(dat,g)$p.value < alpha
    
    c(sum(nullHypothesis & calls),sum(!nullHypothesis & calls))
  
  })
)


1 - (0.95)^(1/1000)




# Bonferroni Correction Exercises #1 (Bonferonni versus Sidak)

alphas <- seq(0,0.25,0.01)
ms <- seq(2, 2000)
m <- 1000*8
m <- 3

k <- alphas / m
plot(alphas, 1 - (1- k)^m, main = "Bonferroni", type = "l")

k <- 1 - (1 - alphas)^(1/m)
plot(alphas, 1 - (1- k)^m, main = "Sidak", type = "l")



# Bonferroni Correction Exercises #2 (Monte Carlo Simulation)

set.seed(1)
m <- 10000
pvals <- runif(m, 0, 1)

BonferroniCorrec <- function(alpha, m){
  return(alpha / m )
}

SidakCorrec <- function(alpha, m){
  return(1 - (1-alpha)^(1 / m))
}

k <- BonferroniCorrec(0.05, m)

sum(pvals <= k)

(fwer <- 1 - (1 - k)^m)

k <- SidakCorrec(0.05, m)

fwer

mean(pvals <= k)
