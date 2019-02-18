gcd <- function(a, b){
  #Breaks in the function due to definitions/criteria
  if(a%%1!=0 | b%%1!=0 | a==0 | b==0){stop("a and b must be nonzero integers!")}
  if(a==b){return(paste("The GCD of ", a, " and ", b, " is ",a, sep=""))}
  
  #Initializing a data frame to be outputted
  output <- as.data.frame(matrix(nrow=1,ncol=2))
  colnames(output) <- c("Euclidean Algorithm", "Linear Combination of r")
  
  #creating necessary variables
  a.b <- c(a,b)
  n <- a.b[which.max(abs(a.b))]
  d <- a.b[which.min(abs(a.b))]
  r <- 1
  q <- 1
  
  #running euclidean algorithm
  while(r!=0){
    r <- n%%d
    q <- (n-r)/d
    output <- rbind(output,c(paste(n," = ",q,"(",d,")"," + ",r, sep=""),paste(r," = ", n, " - ", q, "(",d,")", sep="")))
    n <- d
    d <- r
  }

  #formatting output table before returning
  output <- output[-1,]
  div <- tail(strsplit(output[nrow(output)-1,1],split=" ")[[1]],1)
  output <- rbind(output, c(paste("The GCD of ",a," and ",b," is ",abs(as.numeric(div)),sep="")," "))
  
  #return
  print(output, row.names=FALSE)
}

#test out function
gcd(60,42)
gcd(667,850)
gcd(1855,2345)
