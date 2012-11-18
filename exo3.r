myI = integrate(function(x) exp(-x^2 / 2), 0, 1)

size   = 1E3
alpha  = .05
qalpha = qnorm(1 - alpha/2)

x1  = rnorm(size) 
h1  = sqrt(2 * pi) * (x1 < 1) * (x1 > 0)
I1  = mean(h1)
sd1 = sd(h1)
IC1 = I1 + c(-1,1) * qalpha * sd1 / sqrt(size)

x2  = runif(size)
h2  = exp(-x2^2 / 2)
I2  = mean(h2)
sd2 = sd(h2)
IC2 = I2 + c(-1,1) * qalpha * sd2 / sqrt(size)

precision = (IC2[2] - IC2[1])/(IC1[2] - IC1[1])
#precision =~ 1E-1 the second method is 10 times more precise than the first one with the same number of rvs generated

#[0, 1] is stable by the change of variable x |-> 1 - x and the Lebesgue mesura is invariant by translation,i thus we get the first part of the equality
#using the same arguments integrals of the functions in the right-side of the equality are equals between 0 and 1 and the wanted equality follows 

x3 = runif(size)
h3 = .5 * ( exp(-x3^2 / 2) + exp(-(1 - x3)^2 / 2) )
I3  = mean(h3)
sd3 = sd(h3)
IC3 = I3 + c(-1,1) * qalpha * sd3 / sqrt(size)
