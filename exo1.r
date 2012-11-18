#we use the generic inversion method
reverse_exp_density = function(u, rate) 
  (rate > 0) * (u < 1) * ( - log(u) / rate )
rexp2 = function(size, rate)
  reverse_exp_density(runif(size), rate) 

#we can't generate a infinite vector in R which will be usefull because we don't know k, a priori
#but using the Markov's inequality we know that only precision (here 1% by default) of our samples will be too short
#I'm not sure that 1% is optimal with respect to ratio between the size of samples and the number of times call our function
doubleSamples = function(rate, initialSamples = c(), precision=.01)
  c(initialSamples, rexp(rate/precision, rate))

position = function(rate, samples=doubleSamples(rate)){
  pos = Position(function(t) t >= 1, cumsum(samples))
  if(is(pos, "numeric"))
    return(pos)
  position(rate, doubleSamples(rate, samples))
}

rpois2 = function(size, rate)
  replicate(size, position(rate))

size   = 1E3
lambda = 1
x      = rpois2(size, lambda)
y      = rpois(size, lambda)

par(mfrow=c(1,2))
hist(x, breaks='fd', freq=F, xlim=c(0,10))
hist(y, breaks='fd', freq=F, xlim=c(0,10))
