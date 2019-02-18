#installing and loading required matlab package for the is.prime() function
install.packages('matlab')
library(matlab)

#creating factorize function
factorize <- function(x) {
  #stop the function if x is not a natural number
  if(x%%1!=0 | x <= 0){stop("X must be a natural number!")}
  
  #return x * 1 if x is prime
  if(isprime(x)){return(paste("The prime factorization of", x,"is", x, "x 1. The function ran in approximately 0 seconds."))}
  
  #Start run timer
  start.time <- Sys.time()
  
  #create a list of primes to loop through and divide x by
  n <- c(1:1000)
  primes <- n[as.logical(isprime(n))]
  
  #declare necessary variables for the loop
  factors <- numeric() #a numeric vector to hold X factorized
  factored <- 0    #a variable that stops the loop once X has been factored completely
  
  #loop through and factor the number until all the factors are prime and their product is equal to x
  num.fct <- x
  i <- 1
  while(factored!=1) {
      if(num.fct%%primes[i]==0){
        factors <- c(factors, primes[i])
        num.fct <- num.fct/primes[i]
      }
      else{
        i <- i+1
      }
    if(isprime(num.fct)){
      factors <- c(factors, num.fct)
      factored <- 1
      }
  }
  
  #Stop run timer and format results
  end.time <- Sys.time()
  run.time <- as.numeric(end.time - start.time, digits=5)
  factors <- as.numeric(factors)
  factor.txt <- paste("The prime factorization of", x, "is", paste(factors, collapse=" x "), ".")
  run.txt <- paste("The function ran in", run.time, "seconds.")
  
  #print results
  c(noquote(paste(factor.txt, run.txt)))
}

factorize(115)
factorize(783948)
factorize(1234567890)

#note that when you get to numbers in the billions, the function runs for longer than 20 seconds. 

