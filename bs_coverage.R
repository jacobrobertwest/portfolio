#In this problem you will explore the coverage of a 95% bootstrap confidence interval
#The data we will use in this exploration will be taken from the built-in dataset USArrests, specifically
#the Assaults column. The data is imported and cleaned for you in the following steps

data("USArrests")
USArrests <- USArrests[,2]

#Find the observed mean of the data set
observed <- mean(USArrests)

#create a function that takes in a population dataset x and returns a 95% bootstrap CI on the mean of 
#a random sample of size 10 taken from this population dataset. You can use 100 bootstrap repetitions.

bs.sample <- function(x) {
  samp <- sample(x,10)
  bs <- replicate(100,{
    mean(sample(samp,replace=TRUE))
  })
  bs.ci <- quantile(bs,c(0.025,0.975))
  bs.ci
}

bs.sample(USArrests)

#We are interested in what the coverage of this bootstrap CI method is. Run a Monte Carlo simulation of 
#5000 reps using the function you've created and calculate the proportion of confidence intervals that 
#contain the observed mean. Make sure to assess Monte Carlo error!

coverage.dist <- replicate(5000,{
  ci <- bs.sample(USArrests)
  (observed >= ci[1])&(observed <= ci[2])
})

est <- mean(coverage.dist)
mce <- est + c(-1,1)*qnorm(0.975)*sqrt(est*(1-est)/5000)

c(lwr=mce[1],est=est,upr=mce[2])
