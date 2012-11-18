alpha                 = .28
mu                    = 0
sigma2                = 16
sigma                 = sqrt(sigma2)
number_of_experiences = 1E3
samples_size          = 1E2

estimatedQuantiles = function(N = number_of_experiences, n = samples_size, m = mu, std = sigma)
  replicate(N, quantile(rnorm(n, m, std), probs= c(alpha)))

hist(sqrt(samples_size) * ( estimatedQuantiles() - qnorm(alpha, mu, sigma) ), breaks='fd', freq=F)
curve(dnorm(x, mu, sigma), add=T, col='red')
