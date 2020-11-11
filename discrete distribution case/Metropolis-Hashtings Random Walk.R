target = function(x){
  runif(x)
}

x = rep(0,2000)
x[0] = 3     #initialize; I've set arbitrarily set this to 3
for(i in 1:2001){
  current_x = x[i-1]
  proposed_x = current_x + rnorm(1,mean=0,sd=1) # random walk
  A = target(proposed_x)/target(current_x) 
  
  if(runif(1)<A){
    x[i] = proposed_x       # accept move with probabily min(1,A)
  } else {
    x[i] = current_x        # otherwise "reject" move, and stay where we are
  }
}

plot(x,main="values of x visited by the MH algorithm")

hist(x,xlim=c(0,10),probability = TRUE, main="Histogram of values of x visited by MH algorithm")
xx = seq(0,10,length=100)
lines(xx,target(xx),col="red")
