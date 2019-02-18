#You work as a supply chain manager at a up-and-coming bicycle manufacturing company. One of the main functions of your job is 
#to forecast the demand for your bicycles so you can purchase enough tires from your suppliers in time to sell the completed 
#bikes to your distributors. The function that the supply chain team has derived for forecasting an arbitrary month's bicycle 
#demand is defined by:

#FDm = (1 + (G / 12)) * (ADm-1 + ADm-12) / 2

#Where:
  #FDm is the forecasted demand for the month in question
  #G is the forecasted yearly growth rate
  #ADm-1 is the previous month's actual realized demand
  #ADm-12 is the actual realized demand of the month in question last year.

#It is the end of December 2018 and you are preparing to forecast the demand for January 2019.
#You know that this year's actual realized monthly demands were

AD2018 <- c(jan=200,feb=305,mar=352,apr=327,may=375,jun=407,jul=330,aug=295,sep=275,oct=255,nov=230,dec=450)

#You also know that G (the yearly growth rate) is impossible to predict precisely because of recent volatile growth.  
#However, you know that G has historically followed a normal distribution with a mean of 0.25 and a SD of 0.15.

#PART 1

#Your team would like to estimate the forecasted demand for January 2019 and find a 95% confidence interval on this estimate
#Conduct a monte carlo simulation to find this forecasted demand for January 2019 and its corresponding 95% CI

  G.mean <- 0.25
  G.sd <- 0.15
  Jan18AD <- AD2018[1]
  Dec18AD <- AD2018[12]
  nreps <- 1000
  D.dist <- numeric(nreps)
  
  for (i in 1:nreps){
    G <- rnorm(1,G.mean,G.sd)
    D.dist[i] <- (1+(G/12))*(Jan18AD + Dec18AD)/2
  }
  
  est <- mean(D.dist)
  ci <- est + c(-1,1)*qnorm(0.975)*sd(D.dist)/sqrt(nreps)
  c(estimate=est, lwr=ci[1], upr=ci[2])



#PART 2
#At your company's annual Christmas party, the CEO of the company says that he predicts your company will sell more than 4250 bikes in 2019. 
#You know that your CEO is prone to overconfidence and thus you would like to see how probable his claim is using your team's monthly demand
#formula. You will first create a function that returns the results of one replicate of this simulation, and then you will run this function
#for a few thousand times in your simulation.

#A)
#Complete the function called FD2019 to make it return one possible yearly demand for 2018, or in other words, the sum of each month's demand.
#Remember that February's forecasted demand will be dependent on January's forecasted demand, March's will be dependent on February, and so forth.

FD2019 <- function(G.mean=0.25, G.sd=0.15){
  G <- rnorm(1,G.mean,G.sd)
  YearD <- 0
  for (i in 1:12){
    if(i==1){
      MonthD <- (1+(G/12))*(AD2018[12] + AD2018[i])/2
    }
    else{
      MonthD <- (1+(G/12))*(PreviousMon + AD2018[i])/2
    }
    YearD <- YearD + MonthD
    PreviousMon <- MonthD
  }
  names(YearD) <- "Yearly Demand"
  YearD
}

FD2019()

#B)
#Run a monte carlo simulation study using your FD2019 function. Create a histogram of the results of your monte carlo simulation, and create
#a vertical line on the histogram at 4250, the number of bikes your CEO predicts the company will sell this year.
YearD.dist <- numeric(nreps)
for (j in 1:nreps){
  YearD.dist[j] <- FD2019()
}
hist(YearD.dist)
abline(v=4250,col='red')

#C)
#Using the resulting distribution from your Monte Carlo Simulation, calculate the probability of a yearly demand of 4250.
mean(YearD.dist >= 4250)

#D)
#From this probability, is your CEO being overconfident or not?

#Yes, the probability of us having a demand of 4200 is ~0.02, extremely unlikely.
