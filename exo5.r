lowDividers = function(k){
  l = 1:ceiling(sqrt(k))
  l[! k%%l]
}
dividers = function(k){
  d = lowDividers(k)
  list(
       low  = d,
       high = k/d)
}

scalarSquareMutiple = function(k){
  d = dividers(k)
  any( d$low == d$high )
}

squareMultiple = function(k){
  sapply(k, scalarSquareMutiple)
}

set          = 1:1E7
size         = 1E3
k            = sample(set, size, replace= T)
probability  = mean(squareMultiple(k))
estimated_pi = sqrt(6 / probability)
