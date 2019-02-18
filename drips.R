# You notice that there is a drip under the sink in your bathroom and that the
# number of drips every minute is random.  Specifically, suppose that the
# number of drips in a minute is initially a Poisson random variable with rate
# parameter 'rate'.  (You don't need to know anything about the Poisson
# distribution except that you can sample a random value from the Poisson
# distribution with parameter 'rate' in R with the code: 'rpois(1,rate)'.)
# Unfortunately, the severity of the leak is increasing.  Specifically, with
# each passing minute, the rate parameter increases by x%, where x is a random
# value from the uniform distribution on the interval 0 to 5.  (Note that the
# value of x changes every minute!)  You can sample a random value from this
# uniform distribution using 'runif(1,0,5)'.

TotalDrips <- function(hours, initial = 1) {
  mins <- hours*60
  rate <- initial
  dripsByMin <- numeric(mins)
  for (i in 1:mins) {
    dripsByMin[i] <- rpois(1,rate)
    rate <- rate * (1 + (runif(1,0,5)*0.01))
  }
  sum(dripsByMin)
}

simulate.drips <- function(hrs, init = 1, nReps = 10000) {
  dripsDist <- numeric(nReps)
  for (j in 1:nReps) {
    dripsDist[j] <- TotalDrips(hrs, init)
  }
  dripsDist
}

# Assuming that the initial rate is 1, what is the probability that the total
# number of drips exceeds 700 drips after 2 hours?  Here, and throughout this
# problem, make sure you assess the Monte Carlo error for any Monte Carlo
# estimates you make.  (14 pts.)
SampDist.2Hr <- simulate.drips(2)
Prob.700 <- 1 - ecdf(SampDist.2Hr)(700)
Prob.700
#Monte Carlo Error for 95% confidence
Prob.700 + c(-1,1)*qnorm(0.05/2, lower.tail=FALSE)*sqrt(Prob.700*(1-Prob.700)/length(SampDist.2Hr))
#Probability that number of drips exceeds 700 after 2 hours is 0.62, acknowledging that with Monte Carlo error
#we are 95% sure that this true probability is between 0.61 and 0.63


# Suppose you are interested in testing the null hypothesis that the initial
# rate parameter is 1 versus the alternative hypothesis that the initial rate
# is larger than 1.  You observe 876 drips in 2 hours.  What is the associated
# p-value?  (4 pts.)
SamplDist.H0 <- simulate.drips(2)
PVal.876 <- 1 - ecdf(SamplDist.H0)(876)
PVal.876
#The associated p-value for 876 is around 0.068


# Using a 0.05 level of significance, what conclusion do you make regarding the
# null hypothesis?  (2 pts.)
#Because 0.068 > 0.05, we have insufficient evidence to reject the null hypthosis and thus
#conclude that the initial rate parameter is 1


# What is the power for this test when the initial rate is 1.2?  (8 pts.)
CV <- quantile(SamplDist.H0, prob=0.95)
Power.H0 <- 1 - ecdf(simulate.drips(2,1.2))(CV)
Power.H0
#Power of the test when the initial rate is 1.2 is around 0.42
