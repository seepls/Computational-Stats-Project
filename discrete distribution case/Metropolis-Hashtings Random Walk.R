# target distribution N(10,2). Say we dont know how to sample from it
target <- function(x){
  (1/sqrt(2*pi*2^2))*exp((-(x-10)^2)/(2*2^2))
}

# proposed dist, we know how to sample from it easily
proposed <- function(x){
  rnorm(1,mean=x,sd=1)
}

stored <- rep(NA,10^4)
xk <- 40 # starting point

for(i in 1:10^4){
  xprime <- proposed(xk)#proposed a value 
  ratio <- min(1,target(xprime)/target(xk)) #compare their proba
  # if x_proposed has higher prob, then take the new value else  
  # take the current value 
  accept <- runif(1)< ratio
  stored[i] <- ifelse(accept, xprime, xk)
  xk <-stored[i]
}

plot (stored, type = 'l')
hist(stored, breaks = 30)
